printRIstartupInfo <- function()
{
  version <- packageVersion('easyRasch')
  hello <- paste("This is easyRasch ",version,
                 ". Check for updates at <https://pgmj.github.io/easyRasch/news/index.html> \n",
                 "NOTE: MacOS users with Tahoe 26.1 or later need to run `options(rgl.useNULL = TRUE)` BEFORE loading package `iarm`"
                 ,sep="")
  packageStartupMessage(hello)
}

.onAttach <- function(...) {
  printRIstartupInfo()
}

