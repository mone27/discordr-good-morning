library(discordr)
library(magrittr)
library(lubridate)
library(later)
library(jsonlite)

source(here::here("secrets.R")) # loads token

TEST_CHANNEL_ID = "853900827197177876"
STUDY_CH_ID = "853684583131643934"


bot <- DiscordrBot$new(token)

bot$register_event_handler("MESSAGE_CREATE", function(msg){
  # avoid handling your own messages
  if (msg$author$id == "853907904872841236"){
    return()
  }
  # guard to work only in test channel
  if (msg$channel_id != TEST_CHANNEL_ID){
    return()
  }
  message <- paste("Hi,", msg$author$username, "this is a reply to your message: '", 
                   msg$content, "' (from your R discord bot :nerd:)")
  send_message(message, msg$channel_id, bot)
})

### -- goodmornig messages

morning_time <- "8:00"
timezone <- "CET"

send_goodmorning <- function(){
  embed <- new_embed(
    title = "Good morning",
    description = "Gooood morning! it is time to start a new amazing day :sunrise:",
    image = list(
      url = "https://media1.tenor.com/images/008b1200ac40752b67f93fb541f33dab/tenor.gif?itemid=5082920"
    ))
  send_embed(embed, STUDY_CH_ID, bot)
  
  # reschedule for next day
  schedule_good_morning()
}

schedule_good_morning <- function(){
  notific_time <- (today(timezone) + hm(morning_time)) %>% 
    force_tz(timezone)
  
  if (notific_time < now()) { # time already passed for today, move to tomorrow
    notific_time <- notific_time + days(1)
  }
  
  delay <-  (notific_time - now()) %>% 
    as.duration() %>% 
    as.numeric() # need to convert difftime into seconds
  
  # schedule the good morning using later
  cat("scheduling good morning in", delay, "seconds")
  later(send_goodmorning, delay)
}


# init the good morning 
schedule_good_morning()

enable_console_logging(level=10)
enable_file_logging(level=10)

bot$start()


