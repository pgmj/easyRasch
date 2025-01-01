printRIstartupInfo <- function()
{
  version <- packageVersion('easyRasch')
  hello <- paste("This is easyRasch ",version,". Check for updates at <https://pgmj.github.io/easyRasch/news/index.html>" ,sep="")
  packageStartupMessage(hello)
}

.onAttach <- function(...) {
  printRIstartupInfo()
}
