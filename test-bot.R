source("websocket.R")
source("discord-api.R")
library(lubridate)
library(later)

source(here::here("secrets.R")) # loads token



register_event_handler("MESSAGE_CREATE", function(msg){
  # avoid handling your own messages
  if (msg$author$id == "853907904872841236"){
    return()
  }
  # guard to work only in test channel
  if (msg$channel_id != "853900827197177876"){
    return()
  }
  message <- paste("Hi,", msg$author$username, "this is a reply to your message: '", 
                   msg$content, "' (from your R discord bot :nerd:)")
  send_message(message, msg$channel_id)
})

morning_time <- "8:00:00"

send_goodmorning <- function(){
  message <- "Gooood morning! it is time to start a new amazing day :sunrise:"
  send_message(
    message,
    "853684583131643934" # study tree channel
  )
  # reschedule for next day
  schedule_good_morning()
}

schedule_good_morning <- function(){
  notific_time <- today() + days(1) + hms(morning_time)
  delay <-  (notific_time - now()) %>% 
    as.duration() %>% 
    as.numeric() # need to convert difftime into seconds
  # schedule the good morning using later
  later(send_goodmorning, delay)
}

setup_api(token)
start_bot(token)

# init the good morning 
schedule_good_morning()

