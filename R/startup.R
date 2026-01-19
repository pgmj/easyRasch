printRIstartupInfo <- function()
{
  version <- packageVersion('easyRasch')
  hello <- paste("This is easyRasch ",version,
                 ". Check for updates at <https://pgmj.github.io/easyRasch/news/index.html> \n",
                 "Remember to use `set.seed()` at the start of your code/document.\n",
                 "MacOS users with Tahoe 26.1 or later need to add/run `options(rgl.useNULL = TRUE)` BEFORE loading easyRasch"
                 ,sep="")
  packageStartupMessage(hello)
}

.onAttach <- function(...) {
  printRIstartupInfo()
}

# .onLoad <- function(...) {
#   options(rgl.useNULL = TRUE)
# }
