% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itanalHelpers.R
\name{fmtCqItanal}
\alias{fmtCqItanal}
\title{fmtCqItanal}
\usage{
fmtCqItanal(
  cqItanal,
  groups = "all",
  itemNumber = "all",
  ptBisFlag = 0,
  textColHighlight = "red",
  valueDecPlace = 2
)
}
\arguments{
\item{cqItanal}{An ACER ConQuest itanal list object returned by function \code{getCqItanal}.}

\item{groups}{a vector of group levels to include in the format.}

\item{itemNumber}{a vector of generalised item numbers to format.}

\item{ptBisFlag}{Something.}

\item{textColHighlight}{Something.}

\item{valueDecPlace}{Something.}
}
\value{
A list
}
\description{
helper function to produce nicely formatted summary tables from
a ConQuest Itanal.
}
\examples{
myEx1Sys <- ConQuestSys()
myEx1Sys_itanal <- getCqItanal(myEx1Sys)
myItanalSummary <- fmtCqItanal(myEx1Sys_itanal)
print(myItanalSummary[[1]])
}
