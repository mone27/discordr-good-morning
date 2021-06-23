library(httr)
library(jsonlite)
library(magrittr)

token_api <- NA

setup_api <- function(token){
  token_api <<- token
  header <<- add_headers(Authorization = paste("Bot", token_api),
                        "User-Agent" = "R-Discord-bot/0.1",
                        "Accept" = "application/json")
}

base <- "https://discord.com/api"

paste_url <- function(...){
  paste(..., sep="/")
}

send_message <- function(message, channel_id){
  path <- paste_url("channels", channel_id, "messages")
  body <- list(content=message)
  discord_post_api(path, body)
}

discord_post_api <- function(path, body){
  url <- paste_url(base, path)
  res <- POST(
    url = url,
    config = header,
    body = body,
    encode = "json"
  )
  warn_for_status(res) # in case the request fails
  return(res) 
}
