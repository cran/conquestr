#' @title plotRout
#'
#' @description generates a plot from an 'ACER ConQuest' Rout file. use `ConQuestRout` to read in an Rout
#'  file created by a `plot` command in 'ACER ConQuest'.
#'
#' @param myRout an R object created by the `ConQuestRout` function.
#' @param ... additional arguments passed into plotting functions (e.g., `plotZero = TRUE` to control whether the 0 category is plotted in ICC and MCC).
#' @return A ggplot2 object.
#' @examples
#' myPlot<- plotRout(ConQuestRout())
#' \dontrun{
#' # if you run the above example you will have a ggplot2 object, using the default Rout file (an ICC).
#' str(myPlot)
#' }
#' ## to see why we import this, see https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
#' @importFrom rlang .data
#' @export plotRout
plotRout<- function(myRout, ...){
  # TODO despatching - e.g, have a method for each plot type, e.g., plotRout.ICC ...
  UseMethod("plotRout", myRout)

}

#' @rdname plotRout
#' @importFrom stats setNames
#' @export
plotRout.TestInfo<- function(myRout, ...){

  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- length(levels(myRoutDf$Series))

  #colour palette
  myColours<- data.frame(
    myInt = c(0:7),
    myHex = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
  )

  # This has to go back to merging on colour
  myRoutDf<- merge(myRoutDf, myColours, by.x = "PointColour", by.y = "myInt", all.x = TRUE)
  myRoutDf<- merge(myRoutDf, myColours, by.x = "LineColour", by.y = "myInt", all.x = TRUE)
  names(myRoutDf)[names(myRoutDf) == "myHex.x"]<- "myHex_point"
  names(myRoutDf)[names(myRoutDf) == "myHex.y"]<- "myHex_line"

  # get manual fill
  myFillDf<- unique(myRoutDf[ , c(grep("^Series\\s|myHex_point", names(myRoutDf)))])
  myFill<- setNames(myFillDf$myHex_point, myFillDf$`Series Name`)

  # get manual colour
  myColDf<- unique(myRoutDf[ , c(grep("^LineColour|myHex_point", names(myRoutDf)))])
  myCol<- setNames(myColDf$myHex_point, as.factor(myColDf$LineColour))

  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$x, y = .data$y, colour = as.factor(.data$`Series Name`))) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(labels = names(myFill), values = myFill) +
    # ggplot2::theme(legend.position = "bottom") + # put legend in bottom of plot
    ggplot2::theme(legend.text = ggplot2::element_text(size=8)) +
    ggplot2::labs(
      title = myRout$GraphTitleText,
      x = myRout$xAxisLabelText,
      y = myRout$yAxisLabelText,
      subtitle = myRout$GraphSubTitleText
    ) +
    ggplot2::theme(
      # axis.text.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank()
    )

  return(myPlot)
}

#' @rdname plotRout
#' @export
plotRout.InformationWithLatentDist<- function(myRout, ...){
  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- length(levels(myRoutDf$Series))
  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$y, y = .data$x, colour = as.factor(.data$`Series Name`))) +
    ggplot2::geom_point(data = subset(myRoutDf, as.logical(myRoutDf$DrawPoints) & myRoutDf$`Series Name` == "Case Distribution")) +
    ggplot2::geom_line(data = subset(myRoutDf, as.logical(myRoutDf$DrawPoints) & myRoutDf$`Series Name` == "Case Distribution")) +
    ggplot2::geom_line(data = subset(myRoutDf, myRoutDf$`Series Name` == "Information")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") + # put legend in bottom of plot
    ggplot2::theme(legend.text = ggplot2::element_text(size=8)) +
    ggplot2::labs(
      title = myRout$GraphTitleText,
      x = myRout$yAxisLabelText,
      y = ggplot2::element_blank()
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank()
    )

  return(myPlot)
}


#' @rdname plotRout
#' @importFrom stats setNames
#' @export
plotRout.ICC<- function(myRout, ...){
  # create df of series
  myArgs<- c(...)
  if(!"plotZero" %in% names(myArgs)){
    plotZero<- FALSE
  } else {
    plotZero<- TRUE
  }

  myRoutDf<- routPointsToDf(myRout)
  myRoutDf<- subset(myRoutDf, as.logical(myRoutDf$DrawSeries))
  myRoutDf$Series<- as.numeric(myRoutDf$Series)
  myRoutDf$PointColour<- as.numeric(myRoutDf$PointColour)
  myRoutDf$LineColour<- as.numeric(myRoutDf$LineColour)
  myNumSeries<- max(myRoutDf$Series)
  if(!plotZero){
    if(myNumSeries == 3) myRoutDf<- myRoutDf[ myRoutDf$Series != 1, ]
  }
  # if(myNumSeries > 12){
  #   message("A max of 12 series are supported, some series have been deleted")
  #   for(i in 1:(myNumSeries-12)){
  #     myRout$Series[[i]]$DrawSeries<- FALSE
  #   }
  #   myRoutDf<- routPointsToDf(myRout)
  #   myRoutDf<- subset(myRoutDf, as.logical(myRoutDf$DrawSeries))
  #   myRoutDf$Series<- as.numeric(myRoutDf$Series)
  #   myRoutDf$Series<- myRoutDf$Series - (myNumSeries-12) # correct number of series such that 1 is the first series (required for merge below)
  #   myNumSeries<- max(myRoutDf$Series)
  # }

  #colour palette
  myColours<- data.frame(
    myInt = c(0:7),
    myHex = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666")
  )

  # This has to go back to merging on colour
  myRoutDf<- merge(myRoutDf, myColours, by.x = "PointColour", by.y = "myInt", all.x = TRUE)
  myRoutDf<- merge(myRoutDf, myColours, by.x = "LineColour", by.y = "myInt", all.x = TRUE)
  names(myRoutDf)[names(myRoutDf) == "myHex.x"]<- "myHex_point"
  names(myRoutDf)[names(myRoutDf) == "myHex.y"]<- "myHex_line"

  # get manual fill
  myFillDf<- unique(myRoutDf[ , c(grep("^Series\\s|myHex_point", names(myRoutDf)))])
  myFill<- setNames(myFillDf$myHex_point, myFillDf$`Series Name`)

  # get manual colour
  myColDf<- unique(myRoutDf[ , c(grep("^LineColour|myHex_point", names(myRoutDf)))])
  myCol<- setNames(myColDf$myHex_point, as.factor(myColDf$LineColour))

  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(
      x = .data$x,
      y = .data$y,
      group = .data$`Series Name`,
      fill = .data$`Series Name`,
      linetype = .data$LineStyle,
      colour = as.factor(.data$LineColour)
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(size = ifelse(as.logical(.data[["DrawPoints"]]), 1, -1)), shape=21) +
    ggplot2::scale_linetype_manual(name = "", values = c(1,2), labels = c("Model Probability", "Observed Probability")) +
    ggplot2::scale_size_continuous(range = c(-1, 1)) +
    ggplot2::scale_fill_manual(name = "", values = myFill) +
    ggplot2::scale_colour_manual(values = myCol) +
    ggplot2::guides(size = "none", point = "none", colour = "none") +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + # y axis is a probability
    ggplot2::theme(legend.position = "bottom") + # put legend in bottom of plot
    ggplot2::theme(legend.text = ggplot2::element_text(size=8)) +
    ggplot2::labs(
      title = myRout$GraphTitleText,
      x = myRout$xAxisLabelText,
      y = myRout$yAxisLabelText,
      subtitle = myRout$GraphSubTitleText,
      caption = paste0(myRout$DifficultyLabelText, " , ", myRout$FitLabelText)
    )

  return(myPlot)
}


#' @rdname plotRout
#' @export
plotRout.MCC<- function(myRout, ...){
  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- max(as.numeric(myRoutDf$Series))
  if(myNumSeries == 3) myRoutDf<- myRoutDf[ myRoutDf$Series != "1", ]

  #colour pallette
  myColours<- data.frame(
    myInt = c(0:8),
    myHex = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6")
  )

  myRoutDf<- merge(myRoutDf, myColours, by.x = "PointColour", by.y = "myInt", all.x = TRUE)
  myRoutDf<- merge(myRoutDf, myColours, by.x = "LineColour", by.y = "myInt", all.x = TRUE)
  names(myRoutDf)[names(myRoutDf) == "myHex.x"]<- "myHex_point"
  names(myRoutDf)[names(myRoutDf) == "myHex.y"]<- "myHex_line"

  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(ggplot2::aes(linetype = .data$LineStyle, colour = .data$`myHex_line`)) +
    ggplot2::geom_point(data = subset(myRoutDf, as.logical(myRoutDf$DrawPoints)), ggplot2::aes(colour = .data$`myHex_point`)) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + # y axis is a probability
    ggplot2::theme(legend.position = "bottom") + # put legend in bottom of plot
    ggplot2::theme(legend.text = ggplot2::element_text(size=8)) +
    ggplot2::labs(
      title = myRout$GraphTitleText,
      x = myRout$xAxisLabelText,
      y = myRout$yAxisLabelText,
      subtitle = myRout$GraphSubTitleText,
      caption = paste0(myRout$DifficultyLabelText, " , ", myRout$FitLabelText)
    )

  return(myPlot)
}


#' @rdname plotRout
#' @export
plotRout.default<- function(myRout, ...){

  print("current class of Rout file not supported - using default method")

  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- length(levels(myRoutDf$Series))
  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$`Series Name`)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.text = ggplot2::element_text(size=8)) +
    ggplot2::labs(
      title = myRout$GraphTitleText,
      x = myRout$xAxisLabelText,
      y = myRout$yAxisLabelText,
      subtitle = myRout$GraphSubTitleText,
      caption = paste0(myRout$DifficultyLabelText, " , ", myRout$FitLabelText)
    )

  return(myPlot)
}

# library(ggplot2)
#
# # useful for debug
# tmp1<- ConQuestRout()
# tmp2<- routPointsToDf(tmp1)
#
# ggplot(tmp2, ggplot2::aes(x = x, y = y, colour = `Series Name`)) +
#   geom_line() +
#   geom_point(data = subset(tmp2, as.logical(DrawPoints)))
# # etc


