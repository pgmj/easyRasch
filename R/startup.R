printRIstartupInfo <- function()
{
  version <- packageVersion('easyRasch')
  hello <- paste("This is easyRasch ",version,
                 ". Check for updates at <https://pgmj.github.io/easyRasch/news/index.html> \n",
                 "MacOS users with Tahoe 26.1 or later need to add/run `options(rgl.useNULL = TRUE)` BEFORE loading iarm/easyRasch"
                 ,sep="")
  packageStartupMessage(hello)
}

.onAttach <- function(...) {
  printRIstartupInfo()
}

