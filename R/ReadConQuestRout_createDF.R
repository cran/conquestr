#' @title routPointsToDf
#' @param myRout An 'ACER ConQuest' Rout file created by a call to 'plot' in 'ACER ConQuest'.
#' @return A data frame containing the series that make up the plot
#' @keywords internal
routPointsToDf <- function(myRout) {
  # coerces points to list of matrices, nseries long
  myTempList <- list()
  for (i in seq_len(myRout$NSeries)) {
    tmpName <- stringr::str_wrap(myRout$Series[[i]]$Name, 25)
    tmpMat <- matrix(unlist(myRout[["Series"]][[i]][["Points"]]), ncol = 4, byrow = TRUE)
    tmpSeries <- matrix(rep(i, length(tmpMat[ , 1])))
    tmpSeriesName <- matrix(rep(tmpName, length(tmpMat[ , 1])))
    tmpSeriesDrawSeries <- matrix(rep(myRout$Series[[i]]$DrawSeries, length(tmpMat[ , 1])))
    tmpSeriesDrawPoints <- matrix(rep(myRout$Series[[i]]$DrawPoints, length(tmpMat[ , 1])))
    tmpSeriesJoinPoints <- matrix(rep(myRout$Series[[i]]$JoinPoints, length(tmpMat[ , 1])))
    tmpSeriesLabelPoints <- matrix(rep(myRout$Series[[i]]$LabelPoints, length(tmpMat[ , 1])))
    tmpSeriesLineWidth <- matrix(rep(myRout$Series[[i]]$LineWidth, length(tmpMat[ , 1])))
    tmpSeriesPointColour <- matrix(rep(myRout$Series[[i]]$PointColour, length(tmpMat[ , 1])))
    tmpSeriesLineColour <- matrix(rep(myRout$Series[[i]]$LineColour, length(tmpMat[ , 1])))
    tmpSeriesPointStyle <- matrix(rep(myRout$Series[[i]]$PointStyle, length(tmpMat[ , 1])))
    tmpSeriesLabelStyle <- matrix(rep(myRout$Series[[i]]$LabelStyle, length(tmpMat[ , 1])))
    tmpSeriesLineStyle <- matrix(rep(myRout$Series[[i]]$LineStyle, length(tmpMat[ , 1])))
    tmpMat1 <- cbind(
      tmpMat, tmpSeries, tmpSeriesName, tmpSeriesDrawSeries, tmpSeriesDrawPoints, tmpSeriesJoinPoints,
      tmpSeriesLabelPoints, tmpSeriesLineWidth, tmpSeriesPointColour, tmpSeriesLineColour,
      tmpSeriesPointStyle, tmpSeriesLabelStyle, tmpSeriesLineStyle
    )
    myTempList[[i]] <- tmpMat1
  }
  # turns list of matrices to single matrix
  myTmpMat <- myTempList[[1]]
  for (i in seq_len(length(myTempList))) {
    if (i == 1) next
    myTmpMat <- rbind(myTmpMat, myTempList[[i]])
  }
  # turns single matrix in DF
  myTmpDf <- data.frame(myTmpMat)
  names(myTmpDf)<- c(
    names(myRout[["Series"]][[1]][["Points"]][[1]]),
    "Series", "Series Name", "DrawSeries", "DrawPoints", "JoinPoints", "LabelPoints",
    "LineWidth", "PointColour", "LineColour", "PointStyle", "LabelStyle", "LineStyle"
  )
  myTmpDf$x <- as.numeric(as.character(myTmpDf$x))
  myTmpDf$y <- as.numeric(as.character(myTmpDf$y))
  myTmpDf$z <- as.numeric(as.character(myTmpDf$z))

  return(myTmpDf)
}

#' @title routType
#' @param myRout An 'ACER ConQuest' Rout file created by a call to 'plot' in 'ACER ConQuest'.
#' @return A string naming the type of plot
#' @keywords internal
routType <- function(myRout) {
#define PLOTICC 1               // Plot item characteristic curves
#define PLOTCCC 2               // Plot cumulative probability curves
#define PLOTEXPECTED 3          // Plot expected score curves
#define PLOTIINFO 4             // Plot item information
#define PLOTTINFO 5             // Plot test information
#define PLOTTCC 6               // Plot test characteristic curve
#define PLOTMCC 7               // Plot distractors
#define PLOTCONDITIONAL 8       // Plot (adjacent) conditional probability curves
#define PLOTWRIGHTMAP 9         // Plot a wright map
#define PLOTPPWRIGHTMAP 10      // plot a predicted probability wright map
#define PLOTLIKELIHOOD 11       // plot likelihood
#define PLOTHISTORY 12          // plot history
#define PLOTFITMAP 13           // plot map of fit statistics
#define PLOTINFORMATIONMAP 14   // plot information with latent distribution
#define PLOTLOGLIKELIHOOD 15    // plot log likelihood
#define PLOTSCATTER 16          // plot simple scatter
  myType <- myRout$GType
  stopifnot(exprs = {
    typeof(myType) == "integer"
    myType > 0
    myType < 17
  })

  myRoutClass <- switch(myType,
           "1" = "ICC",
           "2" = "CCC",
           "3" = "Expected",
           "4" = "ItemInfo",
           "5" = "TestInfo",
           "6" = "TCC",
           "7" = "MCC",
           "8" = "Conditional",
           "9" = "WrightMap",
           "10" = "ppWrightMap",
           "11" = "Likelihood",
           "12" = "History",
           "13" = "FitMap",
           "14" = "InformationWithLatentDist",
           "15" = "LogLikelihood",
           "16" = "Scatter"
  )
  return(myRoutClass)
}
