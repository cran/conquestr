% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReadConQuestState.R
\name{ReadSysMini}
\alias{ReadSysMini}
\title{ReadSysMini}
\usage{
ReadSysMini(myFile, Dimensions, N, NPlausibles, isDebug)
}
\arguments{
\item{myFile}{An 'ACER ConQuest' \emph{mini} system file created by the \code{put}
command in 'ACER ConQuest' with the option "mini = yes".
The put command must use the option \code{compressed = no}.}

\item{Dimensions}{.}

\item{N}{.}

\item{NPlausibles}{.}

\item{isDebug}{.}
}
\value{
A list containing the data objects created by 'ACER ConQuest'.
}
\description{
Internal function to read an 'ACER ConQuest' system file.
Called by conquestr::ConQuestSys.
}
\seealso{
conquestr::ConQuestSys()
}
