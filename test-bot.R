source("websocket.R")
source("discord-api.R")

source(here::here("secrets.R")) # loads token


register_handler("MESSAGE_CREATE", function(msg){
  # avoid handling your own messages
  if (msg$author$id == "853907904872841236"){
    return()
  }
  # guard to work only in test channel
  if (msg$channel_id != "853900827197177876"){
    return()
  }
  
  print(msg)
  message <- paste("Hi,", msg$author$username, "this is a reply to your message: `", 
                   msg$content, "` (from your R discord bot :nerd:)")
  send_message(message, msg$channel_id)
})

start_bot(token)

