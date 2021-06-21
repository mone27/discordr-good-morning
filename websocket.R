library(websocket)
library(httr)
library(jsonlite)
library(magrittr)
library(later)

## TODOS

# need to handle connection resuming 
# https://discord.com/developers/docs/topics/gateway#resuming

# this could be cached, for simplicity it is not

### --- globals

callbacks <- list()
token <- NA
saved_ready <- NA
ws <- NA

### --- external interface

# think about moving it to a R6 object?
register_handler <- function(event_type, callback){
  if (typeof(callback) != "closure"){stop("Second argument needs to be a function")}
  callbacks[[event_type]] <<- callback
}


start_bot <- function(token){
  
  token <<- token
  
  # need to initialize websocket
  
  ws_endpoint <- get_ws_endpoint()
  
  
  ws <<- WebSocket$new(ws_endpoint, autoConnect = FALSE)
  
  ws$onOpen(function(event) {
    cat("Connection opened\n")
  })
  ws$onMessage(function(event) {
    receiver_switcher(event$data)
  })
  ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
  })
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  
  #connect to websocket to start bot
  ws$connect()
  
}

### --- 

get_ws_endpoint <- function(){
  get_bot_url <- "https://discord.com/api/gateway"
  res <- GET(url = get_bot_url)
  content(res, "parsed")$url %>% 
    modify_url(query=list(encoding = "json", v=9))
}



### --- code to handle Discord websocket events

# need to keep track of last "s" sent by discord
last_s <- "Null"

# OP code reference
OP_HELLO <- 10 #first message received
OP_HACK <- 11 # acknowledge heartbeat
OP_HB <- 1 # heartbeat
OP_IDENTIFY <- 2 # identify used at bot setup
OP_DISPATCH <- 0 # receiving events from Discord


### -- websocket setup for Discord

send_payload <- function(opcode, data){
  payload <- toJSON(list(op=opcode, d=data), auto_unbox = T)
  cat("sending payload")
  print(payload)
  ws$send(payload)
}


send_heartbeat <- function(){
  send_payload(OP_HB, data=last_s)
}


receiver_switcher <- function(msg){
  msg <- fromJSON(msg)
  cat(paste(Sys.time(), "Received msg:\n"))
  #print(msg)
  
  # need to update the last s for the heartbeat
  # may need to find a better place for this
  last_s <<- msg$s
  
  if (msg$op == OP_HELLO){
    handle_hello(msg)
  }
  if (msg$op == OP_HB){
    send_heartbeat()
  }
  if(msg$op == OP_DISPATCH){
    event_switcher(msg)
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
    print("sending heartbeat")
    send_payload(OP_HB, data=last_s)
    # set up function to send next heartbeat to keep connection open
    later(send_heartbeat, interval)
  }
  
  # send first heartbeat
  send_heartbeat()
  
  # send the intent to start receiving events
  send_intent()
}

send_intent <- function() {
  print("sending intent")
  intent_num <- 512 # This is 1 << 9 or GUILD_MESSAGES
  data <- list(
    token = token,
    properties = list(
      `$os` = "linux",
      `$browser` = "R",
      `$device` = "R"
    ),
    intents = intent_num
  )
  send_payload(OP_IDENTIFY, data)
}

### --- event management

handle_ready <- function(ready_event){
  saved_ready <<- ready_event
}

register_handler("READY", handle_ready)


event_switcher <- function(event){

  event_name <- event$t
  
  if (event_name %in% names(callbacks)){
    callbacks[[event_name]](event$d)
  }
}


