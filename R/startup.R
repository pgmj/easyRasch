printRIstartupInfo <- function()
{
  version <- packageVersion('easyRasch')
  hello <- paste("This is easyRasch ",version,
                 ". Check for updates at <https://pgmj.github.io/easyRasch/news/index.html> \n",
                 "Remember to use `set.seed()` at the start of your code/document."
                 ,sep="")
  packageStartupMessage(hello)
}

.onAttach <- function(...) {
  printRIstartupInfo()
}
