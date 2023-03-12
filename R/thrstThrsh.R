#' @title thrstThrsh
#'
#' @description Generates Thurstonian Thresholds (sometimes called _gammas_) to an item.
#'   Thurstonian thresholds are the location on the trait/scale at which the cumulative probability
#'   of being in category k, or any higher category equals some probability (usually 0.5, the default).
#'   Thurstonian thresholds are considered a way of describing the difficulty of polytomously scored
#'   items and are usually the value used in visualisations like Wright maps.
#'   Thurstonian thresholds can only be calculated for items where response categories are scored such
#'   that each category can be placed in an order increasing scores (e.g., no ties as per the Ordered Partition model)
#'
#' @param myItem A matrix of parameters for a single item of the structure used in `simplef`
#'   (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#' @param threshP The probability at which the threshold is calculated (defaults to the usual value of 0.5)
#' @param minTheta The lower-bound starting value of the split-half search used to find the threshold for the category.
#' @param maxTheta The upper-bound starting value of the split-half search used to find the threshold for the category.
#' @param convC The convergence criteria used to determine when the threshold has been found. The difference between
#'   `threshP` and the cumulative probability of the category and any higher category at the current value of theta
#'   (the current threshold being tested).
#' @return A k-1 by 1 matrix with Thurstonian thresholds for this item.
#'    Values are NA when the threshold cannot be calculated.
#' @examples
#' myItem <- matrix(
#'   c(
#'     0, -0.58    ,  0     , 1,  # delta+tau   thurst thresh (gamma)
#'     1, -0.58    ,  0.776 , 1,  # 0.196       -1.14
#'     2, -0.58    , -0.697 , 1,  # -1.277      -0.93
#'     3, -0.58    , -0.629 , 1,  # -1.209      -0.64
#'     4, -0.58    ,  0.55  , 1   # -0.03        0.25
#'     ), ncol =4, byrow=TRUE
#'  )
#' thrstThrsh(myItem)
thrstThrsh <- function(myItem, threshP = 0.5, minTheta = -20, maxTheta = 20, convC = 0.00001) {

  if (is.unsorted(myItem[ , 1], strictly = TRUE)) {
    warning("scores for item must be strictly increasing")
    result[ , 1] <- NA
    return(result)
  }

  maxK <- length(myItem[, 1])
  result <- matrix(NA, maxK - 1, 1)

  # iter through each threshold (k-1) for this i
  for (k in seq(length(result))) {
    TminTheta <- minTheta
    TmaxTheta <- maxTheta
    tCount <- 1
    cumPk <- 1
    lastMove <- ""
    while (!isTRUE(all.equal(cumPk, threshP, tolerance = convC)))
    {
      theta <- mean(c(TminTheta, TmaxTheta))
      # 1-cumsum(simplep) returns the cumulative probability of k=1...k in rows k=0...k-1
      # this relies on ordered and increasing scoring
      cumPk <- 1 - cumsum(simplep(theta, myItem))[k]
      # prob too large, bring the max value inwards
      if (cumPk > threshP)
      {
        TmaxTheta <- theta
        lastMove <- "max"
      } else
      if (cumPk < threshP) # prob too small, bring the min value inwards
      {
          TminTheta <- theta
          lastMove <- "min"
      }
      # if the distance between TmaxTheta and TminTheta is getting small AND the distance between cumPk and threshP 
      # is not, we need to bump out the opposite of lastMove
      if (abs(TmaxTheta - TminTheta) < convC * 2 & abs(cumPk - threshP) > convC * 2)
      {
        if (lastMove == "")
        {
          TminTheta <- TminTheta + 8
          TmaxTheta <- TmaxTheta + 8
        }
        if (lastMove == "max") TminTheta <- TminTheta - 8
        if (lastMove == "min") TmaxTheta <- TmaxTheta + 8
      }

      tCount <- tCount + 1
      if (tCount > 999) {
        warning("problem with item, more than 1000 iters")
        result[ , 1] <- NA
        return(result)
      }
    }
    result[k] <- theta
  }
  return(result)
}

#' @title itemListToThresholds
#'
#' @description Taskes a list of item parameter matricies and returns a data frame containing
#'   Thurstonian Thresholds (_gammas_) for all items.
#'   Thurstonian thresholds are the location on the trait/scale at which the cumulative probability
#'   of being in category k, or any higher category equals some probability (usually 0.5, the default).
#'   Thurstonian thresholds are considered a way of describing the difficulty of polytomously scored
#'   items and are usually the value used in visualisations like Wright maps.
#'   Thurstonian thresholds can only be calculated for items where response categories are scored such
#'   that each category can be placed in an order increasing scores (e.g., no ties as per the Ordered Partition model)
#'
#' @param myItems A list of item parameter matricies of the structure used in `simplef`
#'   (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#' @param threshP The probability at which the thresholds are calculated (defaults to the usual value of 0.5)
#' @param minTheta The lower-bound starting value of the split-half search used to find the threshold for the category.
#' @param maxTheta The upper-bound starting value of the split-half search used to find the threshold for the category.
#' @param convC The convergence criteria used to determine when the threshold has been found. The difference between
#'   `threshP` and the cumulative probability of the category and any higher category at the current value of theta
#'   (the current proposed value of threshold being tested).
#' @return A data frame including 4 columns:
#'   * id, an integer index reflecting which item this is, in the same order as myItems
#'   * itemid, a string with the names from the items in myItems (NA if item list is not named)
#'   * step, which step does this threshold belong?
#'   * location, the value of the threshold
#' @examples
#' myItem <- matrix(
#'   c(
#'     0, -0.58    ,  0     , 1,  # delta+tau   thurst thresh (gamma)
#'     1, -0.58    ,  0.776 , 1,  # 0.196       -1.14
#'     2, -0.58    , -0.697 , 1,  # -1.277      -0.93
#'     3, -0.58    , -0.629 , 1,  # -1.209      -0.64
#'     4, -0.58    ,  0.55  , 1   # -0.03        0.25
#'     ), ncol =4, byrow=TRUE
#'  )
#' itemListToThresholds(list(myItem))
itemListToThresholds <- function(myItems, threshP = 0.5, minTheta = -20, maxTheta = 20, convC = 0.00001) {

  myDebug <- FALSE

  myCounter <- 1
  tempIds <- list()
  tempNames <- list()
  tempSteps <- list()
  tempThresh <- list()

  if (is.null(names(myItems))) {
    names(myItems) <- seq_along(length(myItems))
  }

  for (item in myItems) {
    itemLen <- length((2:length(item[ , 2]))) # how many thresholds for this item
    tempIds[[myCounter]] <- rep(myCounter, itemLen)
    tempNames[[myCounter]] <- rep(names(myItems)[myCounter], itemLen)
    tempSteps[[myCounter]] <- (2:length(item[ , 2])) - 1
    # TODO: wrap this is in a try to catch wanrings, and then print more useful warnings
    # for example, which item was the problem and why?
    # tempThresh will just generically say too many iters, or not ordered cats.
    tempThresh[[myCounter]] <- thrstThrsh(item, threshP, minTheta, maxTheta, convC)

    if (myDebug) print(paste("complete item", myCounter, " of ", length(myItems)))

    myCounter <- myCounter + 1
  }

  resultIds <- unlist(tempIds)
  resultNames <- unlist(tempNames)
  resultSteps <- unlist(tempSteps)
  resultThresh <- unlist(tempThresh)

  resultDf <- data.frame(
    id = as.integer(resultIds),
    itemid = as.character(resultNames),
    step = as.integer(resultSteps),
    location = as.numeric(resultThresh)
  )

  return(resultDf)
}

#' @title itemListToDeltaDots
#'
#' @description Taskes a list of item parameter matricies and returns a data frame containing
#'   delta dots. This is mostly an internal function used, for example to plot items
#'   on Wright Maps.
#'
#' @return A data frame including 4 columns:
#'   * id, an integer index reflecting which item this is, in the same order as myItems
#'   * itemid, a string with the names from the items in myItems (NA if item list is not named)
#'   * location, the value of the deta dot
#' @keywords internal
#' @examples
#' myItem <- matrix(
#'   c(
#'     0, -0.58    ,  0     , 1,  # delta+tau   thurst thresh (gamma)
#'     1, -0.58    ,  0.776 , 1,  # 0.196       -1.14
#'     2, -0.58    , -0.697 , 1,  # -1.277      -0.93
#'     3, -0.58    , -0.629 , 1,  # -1.209      -0.64
#'     4, -0.58    ,  0.55  , 1   # -0.03        0.25
#'     ), ncol =4, byrow=TRUE
#'  )
#' thrstThrsh(myItem)
itemListToDeltaDots <- function(myItems) {

  myCounter <- 1
  tempIds <- list()
  tempNames <- list()
  tempDeltaDots <- list()
  for (item in myItems) {
    tempIds[[myCounter]] <- myCounter
    tempNames[[myCounter]] <- names(myItems)[myCounter]
    tempDeltaDots[[myCounter]] <- item[length(item[ , 2]) , 2]
    myCounter <- myCounter + 1
  }

  myDeltaDots <- data.frame(
    id = unlist(tempIds),
    itemid = unlist(tempNames),
    location = unlist(tempDeltaDots)
  )

  return(myDeltaDots)
}