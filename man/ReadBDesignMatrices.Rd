% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadConQuestLibrary.R
\name{ReadBDesignMatrices}
\alias{ReadBDesignMatrices}
\title{ReadBDesignMatrices}
\usage{
ReadBDesignMatrices(myFile, ItemSteps, Items)
}
\arguments{
\item{myFile}{An 'ACER ConQuest' system file.}

\item{ItemSteps}{An integer representation of 'ACER ConQuest' object gItemSteps.}

\item{Items}{An integer representation of 'ACER ConQuest' object gNGins.}
}
\value{
A list
}
\description{
ReadSys Read the B design matrix (A list of length gNGins of lists, one per item. For each item a list of length gItemSteps of matrices).
}
\keyword{internal}
