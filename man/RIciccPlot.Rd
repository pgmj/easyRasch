% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyRasch.R
\name{RIciccPlot}
\alias{RIciccPlot}
\title{Conditional Item Characteristic Curves}
\usage{
RIciccPlot(data, class_intervals = 5, method = "cut", dif = "no", dif_var = NA)
}
\arguments{
\item{data}{Dataframe/tibble with only item response data coded as integers}

\item{method}{Either "cut" (default) or "score" for all possible total scores}

\item{dif}{Defaults to "no". Needs a defined \code{dif_var} if set to "yes"}

\item{dif_var}{An exogenous variable (ie. age group, sex) coded as a factor}

\item{classintervals}{Number of groups to divide respondents into}
}
\description{
A wrapper function to simplify getting CICC curves from \code{iarm::ICCplot()} for
any number of items in the same figure. Uses the \code{patchwork} package, which
also allows for further additions to a plot using for instance:
}
\details{
\code{+ plot_annotation(subtitle = "Some subtitle")}. See \code{?plot_annotation} for
more possibilities.

A useful option is for DIF analysis, which requires two optional settings:
\code{dif = "yes"} and \code{dif_var = your$difvariable}.

Text from \code{?iarm::ICCplot}:

Plots Item Characteristic Curves for dichotomous and polytomous items using
average scores within adjacent class intervals (method="cut").
}
