library(discordr)
library(magrittr)
library(lubridate)
library(later)
library(jsonlite)

source(here::here("secrets.R")) # loads token

TEST_CHANNEL_ID = "853900827197177876"


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
  bot$send_message(message, msg$channel_id)
})

### -- goodmornig messages

morning_time <- "8:00"
timezone <- "CET"

send_goodmorning <- function(){
  message <- "Gooood morning! it is time to start a new amazing day :sunrise:"
  bot$send_message(
    message,
    "853684583131643934" # study tree channel
  )
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



test_embed <- function(){
  embed = new_embed(
    title = "This is an embed",
    video = list(
      url = "https://media.tenor.co/videos/65e2b255e20976f17b8759c512742e49/mp4"
    )
  )
  
  send_embed(embed, TEST_CHANNEL_ID, bot)
}

send_embed <-  function(bot, embed, channel_id){
  path <- paste_url("channels", channel_id, "messages")
  body <- I(
    list(content=unbox("test"), embeds = list(list(title=unbox("This is a test")), list(title=unbox("This is a second test"))
         )))
  bot$discord_post_api(path, body)
}

body <- list(embeds = I(list(
  list(
    title = unbox("This is a test")
  )
)))

toJSON(body)

embed = new_embed(
  title = "This is an embed",
  image = list(
    url = "https://media.tenor.co/videos/65e2b255e20976f17b8759c512742e49/mp4"
  ))

send_embed(embed, TEST_CHANNEL_ID, bot)

tree <- new_embed(
  title = "A tree",
  image = list(
    url = "https://upload.wikimedia.org/wikipedia/commons/e/eb/Ash_Tree_-_geograph.org.uk_-_590710.jpg"
  )
)

send_embed(tree, TEST_CHANNEL_ID, bot)

gif <- new_embed(
  title = "A GIF",
  image = list(
    url = "https://media1.tenor.com/images/d686f241dde8606fd7aeb6cf218bac99/tenor.gif?itemid=18394697"
  )
)

send_embed(gif, TEST_CHANNEL_ID, bot)

bot$start()


