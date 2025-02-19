#' @title genItems
#'
#' @description Generates a list of item parameter matrices for use in
#'   function like `conquestr::genResponses` and `conquestr::informationWrightMap`
#'
#' @param n How many items?
#' @param scores When NULL it is assumed that all items have
#'   integer scoring, increasing for each category k, and beginning from 0.
#'   Otherwise a list where the elements are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the scores).
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#'   * a boolean indicating whether the scores should be forced to be increasing across the response
#'     categories.
#'   * optionally a vector of item numbers to apply scores too. If not provided it is assumed that all items
#'     will be scored.
#' @param deltadots A list that describes the sampling distribution from which
#'   item location paramters are drawn. The elements of the list are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the delta dots).
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#'   If the argument is missing, item location distribution is assumed to be ~U(-2, 2).
#' @param taus A list that describes the sampling distribution from which taus
#'   are drawn. Taus are deviations away from the average item location parameter.
#'   The elements of the list are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the taus).
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#'   * a Boolean indicating whether the taus should be forced to be increasing across the response
#'     category boundaries (that is, no items exhibit disordered thresholds).
#'   * optionally, a vector that describes the number of response categories to apply 
#'     to each of the `n` items being sampled. 
#'     The length of this vector must be equal to `n`.
#'     For example if `n`=10, and the first 5 items are polytomous, 
#'     then a vector of length 10, e.g., c(3, 3, 3, 4, 5, 2, 2, 2, 2, 2).
#'     In this example, the first three items have 3 categories each, the fourth item 
#'     has 4 categories, the fifth item 5 categories, and the last five items are all
#'     dichotomies. When missing, a random vector is generated from 
#'     `sample(c(2:5), 10, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))` to create a 
#'     mix of dichotomous and polytomous items.
#'   When the argument is missing, all items are assumed to be dichotomies.
#' @param discrims A list that describes the sampling distribution from which discrimination
#'   parameters are drawn. 
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the discriminations).
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#'   * a Boolean indicating whether the discriminations are forced to be positive.
#'   * a Boolean indicating whether the discrimination is constant within item. When FALSE
#'     a unique discrimination is sampled for each category 
#'     (that is, this can be one way of specifying the Bock Nominal model).
#'   * optionally a vector of item indices to sample a discrimination for.
#'     The length of this vector must be equal to or less than `n`.
#'     For example if `n`=10, and the user wants to sample discriminations for 
#'     items 1, 5, and 10 then the vector is c(1, 5, 10). All other items will have
#'     a discrimination of 1. Otherwise it is assumed that all items
#'     have sampled discriminations.
#'   When missing all items are assumed to have constant discrimination equal to 1.
#' @param centre A number indicating the value to centre the generated values in deltadots on. 
#'   Typically 0 for identification purposes. 
#'   If NULL then values are left at their generated values (e.g., deviating from the expected mean 
#'   proportional to sampling error).
#' @return A list of item matrices.
#' @seealso [conquestr::simplef()], [conquestr::genResponses()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
#' @importFrom stats runif rnorm
genItems <- function(n, scores = NULL, deltadots, taus, discrims = 1, centre = NULL) {
  # list of item matrices returned
  resultList <- list()

  #
  # SET UP n
  # 
  if (missing(n)) stop("`n` is missing with no default")
  if (!is.integer(n)) {
    n <- as.integer(n)
    if (!is.integer(n)) stop("`n` must be an integer")
  } 
  if (n < 1) stop("`n` must be 1 or greater")
  
  #
  # SET UP DELTA DOTS
  #
  if (missing(deltadots)) {
    deltadots <- list()
    deltadots[[1]] <- "runif"
    tlist <- list()
    tlist[[1]] <- -2
    tlist[[2]] <- 2
    deltadots[[2]] <- tlist
  }

  myDeltaD <- genItems_deltaDots(deltadots, n)

  # centre the delta dots
  if (!is.null(centre)) myDeltaD <- (myDeltaD - mean(myDeltaD)) + centre

  # create a dichotomous, integer scored, 1PL item for each item in `n`
  # we add more categories, scores, and discrims later, as required
  for (i in seq(n)) {
    tmpMat <- matrix(c(0, 0, 0, 0, 1, myDeltaD[i], 0, 1), ncol = 4, byrow = TRUE)
    colnames(tmpMat)<- c("x", "d", "t", "a")
    resultList[[i]] <- tmpMat
  }

  #
  # SET UP TAUS
  #
  if (missing(taus)) {
    isDicho <- TRUE
    isOrdered <- TRUE
    taus <- list()
    taus[[1]] <- "runif"
    tlist <- list()
    tlist[[1]] <- -2
    tlist[[2]] <- 2
    taus[[2]] <- tlist
    taus[[3]] <- TRUE
    taus[[4]] <- rep(2, n)
  } 

  myTauBools <- genItems_taus(taus, n)
  isDicho <- myTauBools[[1]]
  isOrdered <- myTauBools[[2]]
  taus <- myTauBools[[3]]
    
  # this is a temp structure for incomplete tau vectors before we process isOrdered
  myTausPre <- list()
  # this is a list of the tau vectors to be applied for each item
  myTaus <- list()  
  
  if (!isDicho) {
    if (taus[[1]] == "runif") {
      if (!(is.numeric(taus[[2]][[1]]) && is.numeric(taus[[2]][[2]]))) {
        stop("the `min` and `max` arguments in the second element of `taus` must be numeric")
      }
      if (taus[[2]][[1]] > taus[[2]][[2]]) {
        stop("the `min` and `max arguments` in the second element of `taus` are mis-ordered")
      }
      myMinTau <- taus[[2]][[1]]
      myMaxTau <- taus[[2]][[2]]
      for (i in seq(n)) {
        tmpTaus <- runif((taus[[4]][i]-2), myMinTau, myMaxTau)
        myTausPre[[i]] <- tmpTaus
        }
      } 

    if (taus[[1]] == "rnorm") {
      if (!(is.numeric(taus[[2]][[1]]) && is.numeric(taus[[2]][[2]]))) {
        stop("the `mu` and `sd` arguments in the second element of `taus` must be numeric")
      }
      myMuTau <- taus[[2]][[1]]
      mySdTau <- taus[[2]][[2]]
      for (i in seq(n)) {
        # print(paste0("i: ", i, " (taus[[4]][i]-2): ", (taus[[4]][i]-2), " myMuTau: ", myMuTau, " mySdTau: ", mySdTau))
        tmpTaus <- rnorm((taus[[4]][i]-2), myMuTau, mySdTau)
        myTausPre[[i]] <- tmpTaus
      }   
    }

    for (i in seq(n)) {
      tmpTaus <- c(0, myTausPre[[i]])
      # print(paste0("i: ", i, " tmpTaus at start: ", tmpTaus))
      if (taus[[4]][i] > 2) {
        tmpTaus <- c(tmpTaus, -1*sum(tmpTaus))
        if (isOrdered) {
          tmpTaus2 <- tmpTaus[2:length(tmpTaus)]
          tmpTaus2 <- sort(tmpTaus2)
          tmpTaus <- c(0, tmpTaus2)
        }
      }
      myTaus[[i]] <- tmpTaus
      # print(paste0("i: ", i, " tmpTaus at end: ", tmpTaus))
    }
  }

  # now, if polytomies, expand the rows in each tmpMat add then add the taus
  if (!isDicho) {
    for (i in seq(n)) {
      if(taus[[4]][i] < 2) {
        stop(
          "each item must have 2 or more response categories, 
          check the 4th element in `taus`"
        )
      } 
      if (taus[[4]][i] > 2) {
        tmpMat <- resultList[[i]]
        tmpK <- taus[[4]][i]
        for (j in 3:tmpK) {
          thisRow <- c((j-1), tmpMat[2, 2], 0, 1)
          tmpMat <- rbind(tmpMat, thisRow)
        }
        tmpMat[ , 3] <- myTaus[[i]]
        rownames(tmpMat) <- NULL
        resultList[[i]] <- tmpMat
      }
    }
  }


  #
  # SET UP DISCRIMS
  #
  if (missing(discrims)) {
    isDiscrims <- FALSE
    isBock <- FALSE
    isPosDiscrims <- TRUE
    discrims <- list()
    discrims[[1]] <- "runif"
    tlist <- list()
    tlist[[1]] <- -2
    tlist[[2]] <- 2
    discrims[[2]] <- tlist
    discrims[[3]] <- TRUE
    discrims[[4]] <- TRUE
    discrims[[5]] <- NULL
  } else {
    if (!is.list(discrims)) stop("`discrims` must be a list")
    if (length(discrims) < 4) {
      stop("discrims must be at least of length 4")
    }
    if (!discrims[[1]] %in% c("runif", "rnorm")) {
      stop ('only "runif", and "rnorm" currently supported for discrims')
    }
    if (!(is.logical(discrims[[3]]) && is.logical(discrims[[4]]))) {
      stop("Third and fourth elements of `discrims` must be Boolean")
    } 
    if (length(discrims) == 4) {
      discrims[[5]] <- seq(n)
    } else {
      discrims[[5]] <- as.integer(discrims[[5]])
      if (!all(as.integer(discrims[[5]]) == discrims[[5]])) stop("Fifth elements of `discrims` must be all ints")
      if (!length(discrims[[5]]) <= n) stop("Fifth elements of `discrims` must be less than or equal to `n`")
      if (!all(discrims[[5]] %in% seq(n))) stop("Fifth elements of `discrims` must contain item indices in `1:n`")
      if (!all(unique(discrims[[5]]) == discrims[[5]])) {
        stop("Fifth elements of `discrims` must contain unique item indices") 
      }
    }
    if (length(discrims[[5]] > 0)) {
      isDiscrims <- TRUE
    } else {
      isDiscrims <- FALSE
    }
    if(discrims[[3]]) {
      isPosDiscrims <- TRUE
    } else {
      isPosDiscrims <- FALSE
    }
    if(discrims[[4]]) {
      isBock <- FALSE
    } else {
      isBock <- TRUE
    }
    # this is a temp structure for incomplete discrim vectors before we process isPosDiscrims
    myDiscrimPre <- list()
    # this is a list of the discrim vectors to be applied for each item
    myDiscrim <- list()

    if (isDiscrims) {
    if (discrims[[1]] == "runif") {
      if (!(is.numeric(discrims[[2]][[1]]) && is.numeric(discrims[[2]][[2]]))) {
        stop("the `min` and `max` arguments in the second element of `discrims` must be numeric")
      }
      if (discrims[[2]][[1]] > discrims[[2]][[2]]) {
        stop("the `min` and `max arguments` in the second element of `discrims` are mis-ordered")
      }
      myMinDiscrim <- discrims[[2]][[1]]
      myMaxDiscrim <- discrims[[2]][[2]]
      for (i in seq(n)) {
        # by now the item matrices in resultList are the right dimensionality
        thisItemNCats <- length(resultList[[i]][ , 1])
        if (!isBock) {
          tmpDiscrims <- runif(1, myMinDiscrim, myMaxDiscrim)
          tmpDiscrims <- rep(tmpDiscrims, (thisItemNCats-1))
        } else {
          tmpDiscrims <- runif((thisItemNCats-1), myMinDiscrim, myMaxDiscrim)
        }
        myDiscrimPre[[i]] <- tmpDiscrims
      }
    } 
    if (discrims[[1]] == "rnorm") {
      if (!(is.numeric(discrims[[2]][[1]]) && is.numeric(discrims[[2]][[2]]))) {
        stop("the `mu` and `sd` arguments in the second element of `discrims` must be numeric")
      }
      myMuDiscrim <- discrims[[2]][[1]]
      mySdDiscrim <- discrims[[2]][[2]]
      for (i in seq(n)) {
        thisItemNCats <- length(resultList[[i]][ , 1])
        if (!isBock) {
          tmpDiscrims <- tmpDiscrims <- rnorm(1, myMuDiscrim, mySdDiscrim)
          tmpDiscrims <- rep(tmpDiscrims, (thisItemNCats-1))
        } else {
          tmpDiscrims <- rnorm((thisItemNCats-1), myMuDiscrim, mySdDiscrim)
        }
        myDiscrimPre[[i]] <- tmpDiscrims
      }   
    }

    for (i in seq(n)) {
      if (isPosDiscrims) myDiscrimPre[[i]] <- abs(myDiscrimPre[[i]])
      tmpDiscrims <- c(0, myDiscrimPre[[i]])
      # print(paste0("i: ", i, " tmpTaus at start: ", tmpTaus))
      if (i %in% discrims[[5]]) {
        tmpMat <- resultList[[i]]
        tmpMat[ , 4] <- tmpDiscrims
        resultList[[i]] <- tmpMat
      }
    }
  }
  }  

  return(resultList)
}

#' @title genItems_deltaDots
#'
#' @description Internal function to parse deltaDots argument 
#'   to `conquestr::genItems`.
#' @param deltadots A list that describes the sampling distribution from which
#'   item location parameters are drawn. 
#' @param n An integer describing the number of items being generated. 
#' @return A vector of delta dots.
#' @keywords internal
#' @importFrom stats runif rnorm
genItems_deltaDots <- function(deltadots, n) {

  if (!is.list(deltadots)) stop("`deltadots` must be a list") 
  if (!deltadots[[1]] %in% c("runif", "rnorm")) {
    stop("first element in `deltadots` must be either `runif` or `rnorm`")
  }

  if (deltadots[[1]] == "runif") {
    if (!(is.numeric(deltadots[[2]][[1]]) && is.numeric(deltadots[[2]][[2]]))) {
      stop("the `min` and `max arguments` in the second element of `deltadots` must be numeric")
    }
    if (deltadots[[2]][[1]] > deltadots[[2]][[2]]) {
      stop("the `min` and `max arguments` in the second element of `deltadots` are mis-ordered")
    }
    myMin <- deltadots[[2]][[1]]
    myMax <- deltadots[[2]][[2]]
    myDeltaD <- runif(n, myMin, myMax)
  } else if (deltadots[[1]] == "rnorm") {
    myMu <- deltadots[[2]][[1]]
    mySd <- deltadots[[2]][[2]]
    myDeltaD <- rnorm(n, myMu, mySd)
  } else {
    stop ('only "runif", and "rnorm" currently supported for deltadots')
  }

  return(myDeltaD)
}

#' @title genItems_taus
#'
#' @description Internal function to parse taus argument 
#'   to `conquestr::genItems`.
#' @param taus A list that describes the sampling distribution from which
#'   item location parameters are drawn. 
#' @param n An integer describing the number of items being generated. 
#' @return A list
#' @keywords internal
#' @importFrom stats runif rnorm
genItems_taus <- function(taus, n) {

  myResult <- list()

  if (!is.list(taus)) stop("`taus` must be a list")
  if (length(taus) < 3) {
    stop("taus must be at least of length 3")
  }
  if (!taus[[1]] %in% c("runif", "rnorm")) {
    stop ('only "runif", and "rnorm" currently supported for taus')
  }
  if (!is.logical(taus[[3]])) stop("Third element of `taus` must be Boolean")
  if (length(taus) == 3) {
   taus[[4]] <- sample(c(2:5), n, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
  } else {
    taus[[4]] <- as.integer(taus[[4]])
  if (!all(as.integer(taus[[4]]) == taus[[4]])) stop("Fourth elements of `taus` must be all ints")
  if (!length(taus[[4]]) == n) stop("Fourth elements of `taus` must be of length `n`")
  }
  if (any(taus[[4]] > 2)) {
    isDicho <- FALSE
  } else {
    isDicho <- TRUE
  }
  if(taus[[3]]) {
    isOrdered <- TRUE
  } else {
    isOrdered <- FALSE
  }

  myResult[[1]] <- isDicho
  myResult[[2]] <- isOrdered
  myResult[[3]] <- taus

  return(myResult)
}