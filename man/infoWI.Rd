% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infoHelpers.R
\name{infoWI}
\alias{infoWI}
\title{infoWI}
\usage{
infoWI(myItems, myAbilities, type = "empirical")
}
\arguments{
\item{myItems}{A vector of item deltas.}

\item{myAbilities}{A vector of person abilities.}

\item{type}{A character String. Should the test information be calculated empirically
("empirical" - default) or analytically using moments of distribution ("approx").}
}
\value{
A double.
}
\description{
Calculates an index representing the product of a test information function and an ability distribution.
}
\examples{
infoWIOut <- infoWI(runif (10, -2, 3), rnorm(1000, 0, 1))
}
