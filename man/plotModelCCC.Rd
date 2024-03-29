% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGeneral.R
\name{plotModelCCC}
\alias{plotModelCCC}
\title{plotModelCCC}
\usage{
plotModelCCC(item, range = c(-6, 6), by = 0.1, plotZero)
}
\arguments{
\item{item}{Item parameters for a single item.}

\item{range}{Lower and upper bounds to plot over (defaults to c(-6, 6).}

\item{by}{Increment to the sequence along `range``.}

\item{plotZero}{Should the zero category be plotted?
Defaults to \code{FALSE} when item is dichotomous and \code{TRUE} otherwise.}
}
\value{
A ggplot2 object.
}
\description{
Creates a plot of a model implied category characteristic curve.
Note this is not for use with \code{rout} files. See the generic function \code{plotRout} for plotting rout files.
}
\examples{
myItem <- matrix(
  c(
    0, 0, 0, 1,
    1, 1, 0, 1
  ),
  ncol = 4, byrow=TRUE
)
myPlot <- plotModelCCC(myItem)
}
