% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showHelpers.R
\name{getCqRespModel}
\alias{getCqRespModel}
\title{getCqRespModel}
\usage{
getCqRespModel(sysFile)
}
\arguments{
\item{sysFile}{An ACER ConQuest system file read into R using conquestr::ConQuestSys}
}
\value{
A List of data frames. Each data frame is a term in the response model
}
\description{
produces a table of model parameter estimates, errors, fits, and
scaled 2PL estimates if available.
}
\examples{
\dontrun{
myShowRespMod <- getCqRespModel(conquestr::ConQuestSys())
}
}
