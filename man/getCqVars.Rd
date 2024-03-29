% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conquestrFunc.R
\name{getCqVars}
\alias{getCqVars}
\title{getCqVars}
\usage{
getCqVars(myCqs)
}
\arguments{
\item{myCqs}{A system file.}
}
\value{
A data frame.
}
\description{
creates a data frame representation of the variables in the model statement.
Note that steps are not variables.
}
\examples{
\dontrun{
getCqVars(ConQuestSys())
}
}
