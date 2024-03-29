% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infoHelpers.R
\name{testInfoAtTheta}
\alias{testInfoAtTheta}
\title{testInfoAtTheta}
\usage{
testInfoAtTheta(myItems, theta)
}
\arguments{
\item{myItems}{A list of matrices of item parameters of the structure used in \code{simplef}}

\item{theta}{a number.}
}
\description{
Calculates test information at a value of theta given a list of matricies of item parameters
for one or more items.
}
\examples{
anItem <- matrix(c(0,0,0,1,1,1,0,1), nrow = 2, byrow = TRUE)
testInfoAtTheta(list(anItem), 0)
}
