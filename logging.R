library(logging)
basicConfig(level=10)
removeHandler("basic.stdout") # remove default handler
# bot user can enable loggin

enable_console_logging <- function(level=20){
  addHandler(writeToConsole, level=level)
}
enable_file_logging <- function(level=20, file="discordr-logs.log"){
  addHandler(writeToFile, level=level, file=file)
}
