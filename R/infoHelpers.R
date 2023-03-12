#' @title myAbilitiesHelper
#'
#' @description helper function to return attributes of vector of abilities for use in information function and WI.
#'
#' @param myWhat A character string indicating what to check.
#' @param myAbilities A numeric vector of person abilities.
#' @return A Boolean or a double.
#' @keywords internal
myAbilitiesHelper <- function(myWhat = "ok", myAbilities = NA) {
  if (myWhat == "ok") {
    if (length(myAbilities) < 2 || !inherits(myAbilities, "numeric")) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  if (myWhat == "min") {
    return(min(myAbilities))
  }

  if (myWhat == "max") {
    return(max(myAbilities))
  }
}

#' @title myItemHelper
#'
#' @description helper function to return attributes of list of items for use in information function helpers and WI.
#'
#' @param myWhat A character string indicating what to check.
#' @param myItems A list of item matricies.
#' @return A Boolean or a double.
#' @keywords internal
myItemHelper <- function(myWhat = "ok", myItems) {
  if (myWhat == "ok") {
    if (length(myItems) < 1 || !inherits(myItems, "list")) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

#' @title itemInfoAtTheta
#'
#' @description Calculates item information at a value of theta given a set of item parameters for one item.
#'
#' @param myItem A matrix of item parameters of the structure used in `simplef`
#' @param theta A number.
#' @examples
#' anItem <-  matrix(c(0,0,0,1,1,1,0,1), nrow = 2, byrow = TRUE)
#' itemInfoAtTheta(anItem, 0)
itemInfoAtTheta <- function(myItem, theta) {
  myDebug <- FALSE
  tInfo1 <- 0
  tInfo2 <- 0
  if (myDebug) {
    print(paste("theta: ", theta))
    print(paste("my item: ", myItem))
    cat("\n")
  }
  myProbs <- simplep(theta, myItem)
  for (i in seq_along(myProbs)) {
    # note that myItem[1 , 1] == 0, and therefore info in first category is always 0
    tmp_a_time_bNew <- (myItem[i , 1] * myItem[i , 4])
    tInfo1_tmp <- (tmp_a_time_bNew^2 * myProbs[i])
    tInfo2_tmp <- (tmp_a_time_bNew * myProbs[i]) # see https://research.acer.edu.au/measurement/4/
    tInfo1 <- tInfo1 + tInfo1_tmp
    tInfo2 <- tInfo2 + tInfo2_tmp # accumulate info across k
  }
  tInfo2 <- tInfo2^2
  info <- tInfo1 - tInfo2
  return(info)
}

#' @title testInfoAtTheta
#'
#' @description Calculates test information at a value of theta given a list of matricies of item parameters
#'   for one or more items.
#'
#' @param myItems A list of matrices of item parameters of the structure used in `simplef`
#' @param theta a number.
#' @examples
#' anItem <- matrix(c(0,0,0,1,1,1,0,1), nrow = 2, byrow = TRUE)
#' testInfoAtTheta(list(anItem), 0)
testInfoAtTheta <- function(myItems, theta) {
  testInfo <- 0
  for (item in myItems) {
    itemInfo <- itemInfoAtTheta(item, theta)
    testInfo <- testInfo + itemInfo # accumulate info across items
  }
  return(testInfo)
}


#' @title itemInfoOverTheta
#'
#' @description Calculates item information over a rage of theta given a set of item parameters.
#'   Returns a data frame with item information at a discrete set of values of theta. This is
#'   useful for plotting item information functions.
#' 
#'   Note this function is redundant - use testInfoOverTheta and pass a single item as a list.
#'
#' @param myItem A matrix of item parameters of the structure used in `simplef`
#' @param minTheta The smallest value of ability PDF to calculate info and to plot.
#'   Defaults to -6.
#' @param maxTheta The largest value of ability PDF to calculate info and to plot.
#'   Defaults to 6.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @examples
#' anItem <- matrix(c(0,0,0,1,1,1,0,1), nrow = 2, byrow = TRUE)
#' itemInfoOverTheta(anItem)
itemInfoOverTheta <- function(myItem, minTheta = -6, maxTheta = 6, stepTheta = 0.1) {

  infoOverRange <- data.frame(
    theta = seq(minTheta, maxTheta, by = stepTheta),
    info = NA
  )

  for (i in seq(infoOverRange$theta)) {
    infoOverRange$info[i] <- itemInfoAtTheta(myItem = myItem, theta = infoOverRange$theta[i])
  }
  return(infoOverRange)
}


#' @title testInfoOverTheta
#'
#' @description Calculates test information over a range of theta given a list of matricies of item parameters
#'   for one or more items.
#'   Returns a data frame with item information at a discrete set of values of theta.
#'   This is useful for plotting test information functions.
#'
#' @param myItems a list of item parameters of the structure used in `simplef`
#' @param minTheta The smallest value of ability PDF to calculate info and to plot.
#'   Defaults to -6.
#' @param maxTheta The largest value of ability PDF to calculate info and to plot.
#'   Defaults to 6.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @examples
#' anItem <- matrix(c(0,0,0,1,1,1,0,1), nrow = 2, byrow = TRUE)
#' testInfoOverTheta(list(anItem))
testInfoOverTheta <- function(myItems, minTheta = -6, maxTheta = 6, stepTheta = 0.1) {

  infoOverRange <- data.frame(
    theta = seq(minTheta, maxTheta, by = stepTheta),
    info = NA
  )

  for (i in seq(infoOverRange$theta)) {
    infoOverRange$info[i] <- testInfoAtTheta(myItems = myItems, theta = infoOverRange$theta[i])
  }
  return(infoOverRange)
}


#' @title informationWrightMap
#'
#' @description Plots test information function, relative to ability density, and item locations.
#'
#' @param myItems A list of matricies describing item parameters.
#' @param myAbilities A vector of person abilities on one dimension.
#' @param type A character String. Should the test information be calculated empirically ("empirical" - default) or
#'     analytically using moments of distribution ("approx").
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @param scaleInfo A scaling factor to apply to the plot of tesr information. Because ability distribution is a 
#'     PDF with area one, and a test information function has area L, this can make the plot more interpretable. 
#'     Defaults to 1.
#' @param plotItemPoints A character string indicating what item points should be plotted along the x-axis.
#'    similar to the histogram of item locations plotted on a Wrightmap.
#'    Can be "none", "deltadots", "thresholds".
#' @return A ggplot2 object.
#' @examples
#' myDeltaDots <- data.frame(
#'   id = c(1:10),
#'   itemid = paste0("item", 1:10),
#'   delta = rnorm(10)
#' )
#' MyTaus <- data.frame(
#'   id = c(2L, 10L),
#'   itemId = NA,
#'   step = c(1L, 1L),
#'   tau = rnorm(2)
#' )
#' myItemList <- makeItemList(deltaDot = myDeltaDots, tau = MyTaus)
#' myInfoPlot <- informationWrightMap(myItemList, rnorm(1000, 0, 1), minTheta=-5, maxTheta=5)
informationWrightMap <- function(
    myItems, myAbilities, type = "empirical",
    minTheta = NA, maxTheta = NA, stepTheta = NA,
    scaleInfo = 1,
    plotItemPoints = "deltadots"
  ) {
  # error checking
  if (!myAbilitiesHelper("ok", myAbilities)) stop("abilities must be a numeric vector of length > 1")
  if (!myItemHelper("ok", myItems)) stop("myItems must be a list of item paramter matricies")
  if (type != "empirical" && type != "approx") stop("`type` must be one of 'emprical' or 'approx'")
  if (!plotItemPoints %in% c("none", "deltadots", "thresholds")) {
    stop("`plotItemPoints` must be one of 'none', 'deltadots', 'thresholds'")
  }

  # set default arguments if not passed in
  if (is.na(minTheta)) minTheta <- myAbilitiesHelper("min", myAbilities) - 3
  if (is.na(maxTheta)) maxTheta <- myAbilitiesHelper("max", myAbilities) + 3
  if (is.na(stepTheta)) stepTheta <- 0.01

  # flags
  plotItemPointsFlag <- FALSE
  if (plotItemPoints %in% c("deltadots", "thresholds")) plotItemPointsFlag <- TRUE
  # create DF with info over theta
  #
  # TODO: implement type = "approx"
  #

  # get info, abilities, and item points to plot
  infoDf <- testInfoOverTheta(myItems, minTheta, maxTheta, stepTheta)
  casesDf <- data.frame(
    theta = myAbilities
  )
  if (plotItemPointsFlag) {
    if (plotItemPoints == "deltadots") myItemPoints <- itemListToDeltaDots(myItems)
    if (plotItemPoints == "thresholds") myItemPoints <- itemListToThresholds(myItems)
  }

  myPlot <- ggplot2::ggplot(casesDf, ggplot2::aes(x = .data$theta) ) +
    # ABOVE: ability dist
    ggplot2::geom_density(fill = "#717aad") + #ggplot2::aes(y = ..density..), fill="#717aad" ) +
    ggplot2::annotate(
      "text", label = "Ability distribution",
      x = minTheta + 0.05, y = +0.2,
      hjust = 0, size = 4, colour = "#717aad"
    ) +
    # BELOW: info function
    ggplot2::geom_line(data = infoDf, ggplot2::aes(x = .data$theta, y = -(.data$info / scaleInfo))) +
    {
      if (type == "empirical") {
        ggplot2::annotate(
          "text", label = "Information function",
          x = minTheta + 0.05, y = -0.2,
          hjust = 0, size = 4, colour = "black"
        )
      }
    } +
    {
      if (type == "approx") {
        ggplot2::annotate(
          "text", label = "Approximation of information function",
          x = minTheta + 0.05, y = -0.2,
          hjust = 0, size = 4, colour = "black"
        )
      }
     } +
    # plot item locations
    # ggplot2::geom_point(data = myDeltaDots, ggplot2::aes(x = .data$delta, y = 0)) +
    {
      if (plotItemPointsFlag) {
        ggplot2::geom_dotplot(
          data = myItemPoints, aes(x = .data$location),
          fill = "white", stroke = 1, stackdir = "down", #stackdir = "centre" to plot around the x axis
          dotsize = 1,
          binwidth = 0.1,
          method = "histodot"
        )
      }
    } +
    # Theming
    ggplot2::theme_bw() +
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

  return(myPlot)
}



#' @title infoWI
#'
#' @description Calculates an index representing the product of a test information function and an ability distribution.
#'
#' @param myItems A vector of item deltas.
#' @param myAbilities A vector of person abilities.
#' @param type A character String. Should the test information be calculated empirically 
#'   ("empirical" - default) or analytically using moments of distribution ("approx").
#' @return A double.
#' @examples
#' infoWIOut <- infoWI(runif (10, -2, 3), rnorm(1000, 0, 1))
infoWI <- function(myItems, myAbilities, type = "empirical") {
  # error checking
  if (!myAbilitiesHelper("ok", myItems)) stop("abilities must be a numeric vector of length > 1")
  #
  # TODO - update this function to work with item lists
  #
  # if (!myItemHelper("ok", myItems)) stop("items must be a numeric vector of length > 0")
  if (type != "empirical" & type != "approx") stop("type must be one of `emprical` or `approx`")

  # set default arguments if not passed in
  minTheta <- myAbilitiesHelper("min", myItems)
  maxTheta <- myAbilitiesHelper("max", myItems)


  # constants
  kappa <- 1.6
  # kappa <- pi/sqrt(3)

  # NOTE this reproduces much of what is in infoFunc, but instead of calculating over a range of 
  # theta (from min to max, by step), it iterates over the observed values in the vector `abilities`
  # set up results object
  myResults <- data.frame(
    theta = myItems,
    info  = NA
  )
  #
  if (type == "empirical") {
    # data structure for intermediate results
    myProbsMat <- matrix(NA, nrow = length(myItems), ncol = length(myItems))
    myInfoMat <-  matrix(NA, nrow = length(myItems), ncol = length(myItems))
    for (i in seq_along(myItems)) {
      for (j in seq_along(1:length(myItems))) {
        # probability of case at myThetaRange[i] endorsing item of difficulty item[j]
        myProbsMat[i,j] <- (exp(myItems[i]-myItems[j]))/(1+exp(myItems[i]-myItems[j])) 
      }
    }
    myInfoMat <- myProbsMat * (1-myProbsMat) # see 1.3 in Adams and Cloney
    myInfoMatSum <- rowSums(myInfoMat)
    myResults$info <- myInfoMat

    myWI <- sum(myResults$info)/length(myItems)
    return(myWI)

  }
  if (type == "approx") {
    # data structure for intermediate results
    myVar <- var(myAbilities)
    kapSig <- kappa^2 + myVar
    tmpRes <- matrix(NA, ncol = 1, nrow = length(myItems))
    for (i in seq_along(tmpRes))
    {
      tmpRes[i] <- exp(-1/2*(myItems[i]^2/kapSig))
    }
    # old
    #myApproxProbsMat <- matrix(NA, nrow = length(myItems), ncol = length(myItems))
    #for (i in seq_along(myAbilities)) {
    #  for (j in seq_along(1:length(myItems))) {
    #    myApproxProbsMat[i,j] <- (1/(2*pi)) * (sqrt((2*pi)/(kappa^2))) * exp(-(myItems[i]-myItems[j])^2/(2*(kappa^2)))
    #  }
    #}
    #myResults$info <- rowSums(myApproxProbsMat)

    #print(paste0("myVar: ", myVar))
    #print(paste0("kapSig: ", kapSig))
    #print(paste0("tmpRes: ", tmpRes))

    myWI <- 1/(sqrt(2*pi*kapSig))*colSums(tmpRes)
    return(myWI)

  }


}