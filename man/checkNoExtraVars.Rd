% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaningHelpers.R
\name{checkNoExtraVars}
\alias{checkNoExtraVars}
\title{checkNoExtraVars}
\usage{
checkNoExtraVars(data, varNames, except = NULL)
}
\arguments{
\item{data}{Raw data, a data frame.}

\item{varNames}{Vector of valid variable names.}

\item{except}{A vector of variable names to be excluded form the check.}
}
\value{
A list.
}
\description{
Check that the raw data does not include extraneous variables.
}
\keyword{internal}
