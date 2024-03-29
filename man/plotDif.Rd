% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGeneral.R
\name{plotDif}
\alias{plotDif}
\title{plotDif}
\usage{
plotDif(mySysToItemDifDf, myScale = "centred", mySuffixes)
}
\arguments{
\item{mySysToItemDifDf}{An R object of class data frame returned from conquestr::sysToItemDifDf}

\item{myScale}{A string specifying if the item parameter estimates displayed should be "centred" (default),
"scaled" (z scores), or "none" (raw).}

\item{mySuffixes}{a vector of strings specifying the names for the two groups being analysed,
e.g., if the two system files are an analysis of boys and girls, the vector may be \verb{c(_male", "_female")}.}
}
\value{
A ggplot2 object.
}
\description{
Creates a plot (ggplot2 object) of item parameter estimates common to two system files
(e.g., a DIF analysis).
}
\examples{
mySys1 <- ConQuestSys()
mySys2 <- ConQuestSys()
mySysList <- list(mySys1, mySys2)
myDifDf <- sysToItemDifDf(mySysList, mySuffixes = c("_male", "_female"), myDims = "all")
myDifPlot <- plotDif (myDifDf,myScale = "centred", mySuffixes = c("_male", "_female"))
\dontrun{
# if you run the above example you will have the plot in the object `myDifPlot`.
plot(myDifPlot)
}
}
\seealso{
conquestr::sysToItemDifDf()
}
