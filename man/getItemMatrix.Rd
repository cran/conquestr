% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateHelpers.R
\name{getItemMatrix}
\alias{getItemMatrix}
\title{getItemMatrix}
\usage{
getItemMatrix(responses, itemParams, ...)
}
\arguments{
\item{responses}{a data frame made up of rows of vectors of item responses, one for each case.}

\item{itemParams}{a list of item designs (each used in a call to \code{simplef}). Must be of same length as \code{responses}.}

\item{...}{optional arguments, e.g., perfAdj.}
}
\value{
a data frame.
}
\description{
generates an item matrix. Useful for
}
\keyword{internal}
