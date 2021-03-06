% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGeneral.R
\name{plotItemMap}
\alias{plotItemMap}
\title{plotItemMap}
\usage{
plotItemMap(mySys, myDims = "D1", ...)
}
\arguments{
\item{mySys}{An 'ACER ConQuest' system file object created using the conquestr::ConQuestSys function.}

\item{myDims}{A string specifying which specific dimensions should be included. The default is "D1", Specific dimensions are specified by the label "D1" for dimensions 1 etc.
Alternatively, you can specify myDims = "all", though what this produces is not currenlty supported.}

\item{...}{Optional arguments, mostly for debugging, e.g., `setDebug = TRUE` will print temporary data frames.}
}
\value{
A ggplot2 object.
}
\description{
Creates a plot (ggplot2 object) of item parameter estimates and abilities on latent trait.
    Note this is not for use with `rout` files. See the method method plotRout.itemMap to the generic function `plotRout`
}
\examples{
mySys1<- ConQuestSys()
myItemMap<- plotItemMap(mySys1, myDims = "all")
\dontrun{
# if you run the above example you will have the plot in the object `myItemMap`.
plot(myItemMap)
}
}
