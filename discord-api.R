library(httr)
library(jsonlite)
library(magrittr)
source("logging.R")

token_api <- NULL
header <- NULL

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

get_ws_endpoint <- function(){
  res <- discord_get_api("gateway")
  url <- content(res, "parsed")$url
  return(modify_url(url, query=list(encoding = "json", v=9)))
}

discord_get_api <- function(path){
  url <- paste_url(base, path)
  res <- GET(
    url = url,
    config = header
  )
  warn_for_status(res) # in case the request fails
  invisible(res)
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
