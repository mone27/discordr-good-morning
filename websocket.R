library(websocket)
library(httr)
library(jsonlite)
library(magrittr)
library(later)

### --- globals

event_handlers <- list()
op_handlers <- list()
token_ws <- NA
saved_ready <- NA
ws <- NA
first_connection <- T # needed to know if send identify or resume on hello
# need to keep track of last seq number sent by discord
last_s <- "Null"

### --- external interface

# think about moving it to a R6 object?

# register event handler
register_event_handler <- function(event_type, callback){
  stopifnot(is.function(callback))
  event_handlers[[event_type]] <<- callback
}

# register OP handler
register_op_handler <- function(op, callback){
  stopifnot(is.function(callback))
  stopifnot(is.numeric(op)) # OP Codes must be integers
  op_handlers[[as.character(op)]] <<- callback
}


start_bot <- function(token){
  token_ws <<- token
  connect_ws()
}


connect_ws <- function(){
  
  # initialize websocket
  ws_endpoint <- get_ws_endpoint()
  
  ws <<- WebSocket$new(ws_endpoint, autoConnect = FALSE)
  
  ws$onMessage(function(event) {
    receiver_switcher(event$data)
  })
  ws$onClose(on_ws_close)
  
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  
  #connect to websocket to start bot
  ws$connect()
}
### --- setup logger

library(logging)
basicConfig()
addHandler(writeToFile, file="discord_websocket.logs")
removeHandler("basic.stdout")

### ---

get_ws_endpoint <- function(){
  get_bot_url <- "https://discord.com/api/gateway"
  res <- GET(url = get_bot_url)
  content(res, "parsed")$url %>% 
    modify_url(query=list(encoding = "json", v=9))
}



### --- code to handle Discord websocket events

# OP code reference
OP_DISPATCH <- 0 # receiving normal events from Discord
OP_HB <- 1 # heartbeat
OP_IDENTIFY <- 2 # identify used at bot setup
# 3-4 are for updating the guild status, ignored for now
OP_RESUME <- 6 # to ask to resume session
OP_RECONNECT <- 7 # need to reconnected to ws
# 8 request guild member
OP_INVALID_SESSION <- 9 # session is invalid
OP_HELLO <- 10 #first message received
OP_HEARTBEAT_ACK <- 11 # acknowledge heartbeat



### -- websocket setup for Discord

send_payload <- function(opcode, data){
  payload <- toJSON(list(op=opcode, d=data), auto_unbox = T)
  loginfo("Sending message")
  loginfo(payload)
  ws$send(payload)
}


receiver_switcher <- function(msg){
  # parse message
  msg <- fromJSON(msg)
  
  loginfo("Received message")
  loginfo(msg)
  
  update_seq(msg)
  
  # switching for op
  op <- as.character(msg$op)
  if (op %in% names(op_handlers)){
    op_handlers[[op]](msg)
  }
  else {
    warning("Unhandled OPCODE")
    print(msg)
  }
}

update_seq <- function(msg){
  # need to update the last s for the heartbeat and resume
  # may need to find a better place for this
  if (!is.null(msg$s)) { # seems that during ACK s in null avoid saving it
    last_s <<- msg$s
  }
}


# hello is the first message sent by discord on the websocket
# need to setup heartbeat and intent
handle_hello <- function(hello_msg){
  print("handling hello")
  # first thing to do is sending an heartbeat
  interval <- hello_msg$d$heartbeat_interval/1000
  # closure to capture interval
  send_heartbeat <- function(){
    #print("sending heartbeat")
    send_payload(OP_HB, data=last_s)
    # set up function to send next heartbeat to keep connection open
    later(send_heartbeat, interval)
  }
  
  # send first heartbeat
  send_heartbeat()
  
  # send the intent to start receiving events
  if (first_connection){
    send_identify()
  }
  else {
    send_resume()
  }
}
# first connection
register_op_handler(OP_HELLO, handle_hello)

# Heartbeat ACK, for now ignoring it
register_op_handler(OP_HEARTBEAT_ACK, function(msg){})


send_heartbeat <- function(msg){
  send_payload(OP_HB, data=last_s)
}
# respond with heartbeat if requested. Shoul be used rarerly
register_op_handler(OP_HB, send_heartbeat)


send_identify <- function() {
  print("sending intent")
  intent_num <- 512 # This is 1 << 9 or GUILD_MESSAGES
  data <- list(
    token = token_ws,
    properties = list(
      `$os` = "linux",
      `$browser` = "R",
      `$device` = "R"
    ),
    intents = intent_num
  )
  send_payload(OP_IDENTIFY, data)
  first_connection <<- F
}


handle_invalid_session <- function(msg){
  first_connection <<- T # reset connection
  send_identify()
}

register_op_handler(OP_INVALID_SESSION, handle_invalid_session)

send_resume <- function(){
  loginfo("Resuming connection")
  resume <- list(token=token_ws,
                 session_id=saved_ready$session_id,
                 seq = last_s)
  send_payload(OP_RESUME, resume)
}

on_ws_close <- function(event){
  logwarn("Client disconnected with code ", event$code,
      " and reason ", event$reason, "\n", sep = "")
  # trying to reconnect which then will send a resume request
  connect_ws() # this creates a new websocket a reconnects
}

handle_reconnect_request <- function(msg){
  # close and then reopen the websocket
  ws$close()
}


register_op_handler(OP_RECONNECT, handle_reconnect_request)

# Ignoring the signal that the resume is complete
register_op_handler(OP_RESUME, function(msg){})


### --- event management

event_switcher <- function(event){

  event_name <- event$t
  
  if (event_name %in% names(event_handlers)){
    event_handlers[[event_name]](event$d)
  }
}

# normal events
register_op_handler(OP_DISPATCH, event_switcher)

# by default handle only ready event 
handle_ready <- function(ready_event){
  saved_ready <<- ready_event
}

register_event_handler("READY", handle_ready)
