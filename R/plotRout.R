#' @title plotRout
#'
#' @description generates a plot from an 'ACER ConQuest' Rout file. use `ConQuestRout` to read in an Rout
#'  file created by a `plot` command in 'ACER ConQuest'.
#'
#' @param myRout an R object created by the `ConQuestRout` function.
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
plotRout<- function(myRout){
  # TODO despaching - e.g, have a method for each plot type, e.g., plotRout.ICC ...
  UseMethod("plotRout", myRout)

}

#' @rdname plotRout
#' @export
plotRout.InformationWithLatentDist<- function(myRout){
  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- length(levels(myRoutDf$Series))
  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$y, y = .data$x, colour = .data$`Series Name`)) +
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
#' @export
plotRout.ICC<- function(myRout){
  # create df of series
  myRoutDf<- routPointsToDf(myRout)
  myNumSeries<- length(levels(myRoutDf$Series))
  # plot
  myPlot<- ggplot2::ggplot(myRoutDf, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$`Series Name`)) +
    ggplot2::geom_point(data = subset(myRoutDf, as.logical(myRoutDf$DrawPoints))) +
    ggplot2::theme_bw() +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values=stats::setNames(myRoutDf$LineColour, myRoutDf$`Series Name`)) + # maps each series name to a colour (e.g., so in ICC, the empirical line is the same colour as the model line)
    ggplot2::coord_cartesian(ylim = c(0, 1)) + # y axis is a probability
    # ggplot2::theme(legend.position = c(.95, .05), legend.justification = c("right", "bottom"), legend.box.just = "right") + # put legend in plot area, lower right
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
plotRout.default<- function(myRout){
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


