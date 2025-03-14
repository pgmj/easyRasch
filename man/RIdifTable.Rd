% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyRasch.R
\name{RIdifTable}
\alias{RIdifTable}
\title{DIF PCM analysis - requires having set up dif.variables previously}
\usage{
RIdifTable(dfin, dif.var, cutoff = 0.5, table = TRUE)
}
\arguments{
\item{dfin}{Dataframe with item data only}

\item{dif.var}{DIF variable}

\item{cutoff}{Cutoff in item location logit difference for table highlighting}

\item{table}{Set to TRUE = output a table object, and FALSE = dataframe \code{difTablePCM}}
}
\description{
Makes use of the psychotree package, which also allows for interactions
between DIF variables, see \code{RIdifTable2()}.
}
\details{
DIF variables need to be vectors with the same length as the number of rows
in the dataset.

sample usage: RIdifTable(df, dif.age)
}
