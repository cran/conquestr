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
#' @param deltadots A list where the elements are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the delta dots).
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#' @param taus When NULL all items are assumed to be dichotomies.
#'   Otherwise a list where the elements are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the taus).
#'     Or the string "manual" to indicate that a user-defined list of the tau parameters will be
#'     provided.
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function. if the first element in `taus` is "manual" this should be a list of taus to be used
#'     this must be of the correct dimensions - for example, if there are 2 polytomous items being generated,
#'     each with 3 response categories (k), then the list should be of length 2: 2 * (k-  2) =  2 * (3 - 2) =
#'     2 taus. This is because there are k-1 taus per item, and the last tau is always constrained to be the negative
#'     sum of the rest for identification purposes.
#'   * a Boolean indicating whether the taus should be forced to be increasing across the response
#'     category boundaries (that is, enforce that no item exhibits disordered thresholds).
#'   * optionally a vector of item numbers to produce taus for. If not provided it is assumed that all items
#'     are polytomous.
#'   * optionally a vector of response categories to apply to each item. For example if the user indicates that
#'     5 items are polytomous, then a vector of length 5 where the first elements describes the count of response
#'     categories for the first polytomous item, the second element describes the count of response
#'     categories for the second polytomous item, and so on
#' @param discrims When NULL all items are assumed to have constant discrimination equal to 1.
#'   Otherwise a list where the elements are, in order:
#'   * a string naming a distribution function (for example `runif`, `rnorm`) to generate random deviates
#'     from (the discriminations).
#'     Or the string "manual" to indicate that a user-defined list of the discrimination parameters will be
#'     provided.
#'   * a list of parameters to pass to the distribution function (for example, for `runif`, a list of
#'     length 2 defining "min" and "max"). This list is assumed to be in order to be directly passed into
#'     the function.
#'   * a Boolean indicating whether the discriminations are constant within items or whether each response category
#'     within an item will have its own score should be forced to be increasing across the response
#'     category boundaries (that is, this can be one way of specifying the Bock Nominal model).
#'   * optionally a vector of items to apply a unique discrimination to. Otherwise it is assumed that all items
#'     have unique discriminations.
#' @return A list of item matrices.
#' @seealso [conquestr::simplef()], [conquestr::genResponses()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
#' @importFrom stats runif rnorm
genItems <- function(n, scores = NULL, deltadots, taus = NULL, discrims = 1) {
  # TODO: check inputs are valid

  resultList <- list()

  if (is.null(taus)) {
    isDicho <- TRUE
  } else {
    isDicho <- FALSE
    # later need to check that this is true!
    # after we draw the taus, check that there is > 2 resp cats per item
  }

  if (deltadots[[1]] == "runif") {
    myMin <- deltadots[[2]][1]
    myMax <- deltadots[[2]][2]
    myDeltaD <- runif(n, myMin, myMax)
  } else if (deltadots[[1]] == "rnorm") {
    myMu <- deltadots[[2]][1]
    mySd <- deltadots[[2]][2]
    myDeltaD <- rnorm(n, myMu, mySd)
  } else {
    stop ('only "runif", and "rnorm" currently supported')
  }

  return(NULL)
}

#' @title genResponses
#'
#' @description Generates response vectors for `n` cases to `i` items given
#'   known item parameters, person abilities, and (optionally) other inputs.
#'
#' @param abilities A person by latent-dimension matrix of abilities.
#'   One column per dimension.
#' @param groups A vector of factors of the same length as `abilities` that
#'   allocates each case to a group. Used in perturbR. Defaults to NULL such that
#'   all cases are in the one group.
#' @param itemParams A list of item parameters of the structure used in `simplef`
#'   (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#'   See conquestr::makeItemList for a helper to generate this list.
#' @param BMatrix A simplified B-matrix mapping dimensions (columns) to items (rows).
#'   Or the integer "1" if items are dichotomous and ability is uni-dimensional.
#' @param mcarP A double indicating the proportion of missing data
#'   under the MCAR assumption.
#' @param perturbR A list of lists, where each element of the list refers to one item
#'  and contains a list of elements describing how responses to that item should
#'  be perturbed to model misfit.
#'  Each element of the list should contain, in order:
#'
#'  * item number (int). Which item in `itemParams` is affected,
#'  * type of perturbation (string) to apply. One of
#'     * "discrimination" - increasing or decreases the discrimination
#'     * ...more to come, like "shift"
#'  * perturbation factor (double). When the type is "discrimination", this defines
#'    the scale that the discrimination is increased or decreased. For example, if
#'    the item has discrimination of 1, and the perturbation factor is 1.2, the
#'    resulting probabilities will be calculated assuming the discrimination
#'    is 1 * 1.2 = 1.2.
#'  * pivot point (double), When the type is "discrimination", this defines the
#'      location  around which the perturbation is applied relative to
#'      the delta dot. That is, when the type is "discrimination" and the
#'      "perturbation factor" is > 1, probabilities above the pivot point
#'      will be overestimated (generated responses will higher than expectation)
#'      and probabilities below the pivot point will be underestimated
#'      (generated responses will lower than expectation). When the pivot point
#'      is 0, this calculation happens at the item location parameter (e.g., at the
#'      category boundary).
#'  * group (string). The group found in `groups that should be perturbed.
#'    note that is `groups` is not used in call to `genResponses` than this
#'    value is ignored and all cases' responses are perturbed.
#'
#' @return A matrix, `n` cases by `i` items, of scored item responses.
#' @seealso [conquestr::simplef()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
#' @importFrom stats runif
genResponses <- function(
    abilities, groups = NULL, itemParams,
    BMatrix = 1, mcarP = 0, perturbR = NULL
  ) {

  # for debug only
  isDebug <- FALSE

  # error checking
  if (!is.list(itemParams)) {
    stop(
      "`itemParams` should be a list of item params of the structure used in `simplef`"
    )
  }

  # in unidimensional case myNCases could be a vector so coerce to matrix
  myAbilitiesT <- as.matrix(abilities)
  myNCases <- length(myAbilitiesT[, 1]) # can includes NAs - responses will all be NA
  myNItems <- length(itemParams)
  isMultDim <- FALSE
  if (is.matrix(BMatrix) && ncol(as.matrix(BMatrix)) > 1) {
      isMultDim <- TRUE
      if (!all(rowSums(BMatrix) == 1)) stop("only between-item designs currently allowed")
  }
  # handle groups
  isGroups <- FALSE
  if (is.null(groups)) {
    myGroups <- factor(rep(1, myNCases))
  } else {
    if (!is.factor(groups)) groups <- as.factor(groups) # attempt to cast to factor
    if (!is.factor(groups) || length(groups) != myNCases) {
      stop(
        "groups must be the same length as the number of cases in abilities
        and must be of type factor"
      )
    }
    isGroups <- TRUE
    myGroups <- groups
  }
  # handle perturbations
  myNperturb <- 0 # how many perturbed items?
  myPerturbI <- NULL # which items are perturbed?
  if (!is.null(perturbR)) {
    myNperturb <- length(perturbR)
    for (i in seq(perturbR)) {
      tmpI <- as.integer(perturbR[[i]][1])
      if (!is.integer(tmpI)) {
        stop(
          paste0(
            "item number in perturbR must be integer, problem with item ",
            i,
            " and value: ",
            tmpI
          )
        )
      }
      myPerturbI <- c(myPerturbI, tmpI)
    }
  }

  # Now start the real work
  # create a temp response matrix, n cases by i items
  myResponses <- matrix(NA, myNCases, myNItems)
  # create a temp expectation matrix, n cases by i items
  myExpected <- matrix(NA, myNCases, myNItems)
  # loop over cases and items and calculate expectation and response
  for (case in seq(myNCases)) {
    thisGroup <- myGroups[case]
    if (!isGroups) thisGroup <- NULL
    for (item in seq(myNItems)) {
      if (isMultDim) {
        thisDim <- which(BMatrix[item, ] == 1)
      } else {
        thisDim <- 1
      }
      thisItemPeturb <- FALSE
      thisPeturb <- NULL # the actual list of perturbation info for this item
      if (isDebug) print(paste0("working on item: ", item))
      if (isDebug) print(paste0("is item: ", item, " in myPerturbI: ", myPerturbI, " ?", item %in% myPerturbI))
      if (item %in% myPerturbI) {
        thisItemPeturb <- TRUE
        for (i in seq(perturbR)) {
          if (as.integer(perturbR[[i]][1]) == item) thisPeturb <- perturbR[[i]]
        }
      }
      if (thisItemPeturb && is.null(thisPeturb)) {
        stop("could not find the info in perturbR for this item")
      }
      if (is.na(myAbilitiesT[case, thisDim])) {
        resp <- NA
        prob <- NA
      } else {
        prob <- simplef(myAbilitiesT[case, thisDim], itemParams[[item]])
        if(thisItemPeturb) {
          if (isDebug) print(paste0("This case's group: '", thisGroup, "' this items perturbed group: '", thisPeturb[5], "'"))
          resp <- perturbResp(
            myAbilitiesT[case, thisDim], itemParams[[item]],
            thisGroup, thisPeturb
          )
        } else {
          resp <- itemResp(myAbilitiesT[case, thisDim], itemParams[[item]])
        }
      }
      myResponses[case, item] <- resp
      myExpected[case, item] <- prob
      if (isDebug && case < 11 && item < 3) {
        print(
          paste0(
            "case: ", case,
            " group: ", thisGroup,
            " item: ", item,
            " response: ", resp,
            " expected score: ", round(prob, 2),
            " theta: ", round(myAbilitiesT[case , thisDim], 2)
          )
        )
      }
    }
  }

  # add missing if requested
  if (mcarP > 0) {
    myRand2 <- matrix(runif(myNCases * myNItems), myNCases, myNItems)
    for (r in 1:nrow(myResponses)) {
      for (c in 1:ncol(myResponses)) {
        if (myRand2[r, c] > (1 - mcarP)) {
          myResponses[r, c] <- NA
        }
      }
    }
  }

  return(myResponses)
}

#' @title perturbResp
#'
#' @description Perturbs a response given specified (item) misfit to implied model.
#'
#' @param ability a double - the location of the person on the measure.
#' @param item a matrix - the item parameters in the format required by 
#'   `conquestr::simplef` 
#' @param group a string - the group this case belongs to. If NULL, the case is 
#'   always in this group.
#' @param perturbation A matrix - the description of the way this item should be 
#'   perturbed. In the format required by `conquestr::genResponses` 
#' @return A double - the observed response.
#' @keywords internal
#' @seealso [conquestr::simplef()], [conquestr::genResponses()], 
#'   `browseVignettes("conquestr")`
#' @examples
#' myN <- 250
#' myMean <- 0
#' mySd <- 2
#' myGroups <- c("gfit", "bfit")
#' myAbilities <- rnorm(myN, myMean, mySd)
#' # students with large weights and unperturbed responses (good fit)
#' #   and students with small weights and perturbed responses (bad fit)
#' myData <- data.frame(
#'   ability = myAbilities,
#'   group = factor(sample(x = myGroups, size = myN, replace = TRUE))
#' )
#' myData$weight <- ifelse(myData$group == myGroups[1], 1, 0.001)
#' myItems <- list()
#' myItems[[1]] <- matrix(c(
#'   0, 0, 0   , 1,
#'   1, 1, -0.2, 1,
#'   2, 1, 0.2 , 1
#' ), ncol = 4, byrow = TRUE)
#' myPerturbations<- list()
#' myPerturbations[[1]] <- list()
#' myPerturbations[[1]] <- append(myPerturbations[[1]], 1L)
#' myPerturbations[[1]] <- append(myPerturbations[[1]], "discrimination")
#' myPerturbations[[1]] <- append(myPerturbations[[1]],  0.50)
#' myPerturbations[[1]] <- append(myPerturbations[[1]], 0)
#' myPerturbations[[1]] <- append(myPerturbations[[1]], "bfit")
#' names(myPerturbations[[1]]) <- c("item", "type", "factor", "pivot", "group")
#' myResponses <- genResponses(
#'     abilities = myData$ability, itemParams = myItems, perturbR = myPerturbations,
#'     groups = myData$group
#' )
#' @importFrom stats runif
perturbResp <- function(ability, item, group, perturbation) {

  # Is this case is in the group to be perturbed?
  caseIsInGroup <- FALSE
  if (is.null(group) || group == perturbation[5]) caseIsInGroup <- TRUE

  if (!caseIsInGroup) {
    thisResp <- itemResp(ability, item)
  } else {
    tmpI <- item
    nCats <- length(tmpI[ , 2])
    tFac <- as.numeric(perturbation[3])
    tPivot <- as.numeric(perturbation[4])
    tmpI[, 4] <- tmpI[, 4] * tFac
    tmpI[(2:nCats), 2] <- tmpI[(2:nCats), 2] + tPivot
    thisResp <- itemResp(ability, tmpI)
  }
  return(thisResp)
}


#' @title itemResp
#'
#' @description Returns a probabilistic item response, given one ability and one
#'   set of item parameters.
#'
#' @param myAblty A numeric ability.
#' @param myItem A single item (a matrix k categories by four
#'   (category score, delta dot, tau, discrimination).
#' @return A numeric score, usually an integer in the range
#'   0, 1, 2, ..., k-1. This is taken from the first column of the matrix of
#'   item parameters so may also contain a numeric score.
#' @keywords internal
#' @examples
#' myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#' itemResp(0, myItem)
itemResp <- function(myAblty, myItem) {
  myRand <- runif(1)
  myRspP <- simplep(myAblty, myItem)
  myCdf <- cumsum(myRspP)
  myCatScore <- min(which(myRand < myCdf))
  return(myItem[myCatScore, 1])
}

#' @title rawScore
#'
#' @description returns the adjusted (for zero and perfects) and unadjusted raw scores as well as the maximum score.
#'
#' @param x a vector of scored item responses. Order of item responses is the same as the order of `itemParams`,
#'   missing item responses are scored NA.
#' @param itemParams A list of item params of the structure used in `simplef` (a matrix of k categories by three
#'   (category score, delta dot, tau)). See conquestr::makeItemList for a helper to generate this list.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#' @return a list containing three elements: (1) raw score (unadjusted), (2) raw score (adjusted), (3) maximum score.
#' @keywords internal
rawScore <- function(x, itemParams, perfAdjust = 0.3) {
  myResults <- list()
  if (perfAdjust < 0 || !inherits(perfAdjust, "numeric")) stop("perfAdjust must be numeric and greater than 0")

  # calc raw score
  myRaw <- sum(x, na.rm = TRUE)

  # calc max score
  tmpItemParams <- itemParams[!is.na(x)] # keep the item params with a valid response
  countItems <- length(x[!is.na(x)]) # how many items were responded to?
  if (length(countItems) > 0) {
    maxScore <- 0
    for (i in seq(countItems))
    {
      # this is the max category for this item - NOTE: this may include scores
      tmp <- max(tmpItemParams[[i]][ , 1])
      maxScore <- maxScore + tmp
    }
  } else {
    maxScore <- NA # no items responded to
  }

  # calc adjusted raw score
  myRawA <- myRaw
  if (perfAdjust > 0) {
    if (myRaw == 0) myRawA <- myRaw + perfAdjust
    if (myRaw == maxScore) myRawA <- myRaw - perfAdjust
  }

  myResults[["raw score unadjusted"]] <- myRaw
  myResults[["raw score adjusted"]] <- myRawA
  myResults[["highest score possible"]] <- maxScore

  return(myResults)
}

#' @title thetaScore
#'
#' @description Return score (O-E) at theta. Used in estimation of theta by minimising
#'   _raw score - expected score_ at theta
#'
#' @param theta a scalar value of theta.
#' @param responses a vector of item responses for one case (can contain NA). Responses are in the same order as `items`
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#'
#' @return a double, the score (O-E) at theta.
#' @keywords internal
thetaScore <- function(theta, responses, itemParams, perfAdjust = 0.3) {
  tmpRaw <- rawScore(responses, itemParams, perfAdjust = perfAdjust)[[2]] # observed
  tmpExp <- list()
  for (i in seq(itemParams)) {
    if (is.na(responses[i])) {
      tmpExp[[i]] <- NA
      next
    }
    tmpExp[[i]] <- simplef(theta, itemParams[[i]])
  }
  tmpExpSum <- sum(unlist(tmpExp), na.rm = TRUE) # expected
  return(tmpRaw - tmpExpSum)
}

#' @title cnvrtItemParam
#'
#' @description takes an item in one model's parameterisation and
#'   returns it in another parameterisation.
#'
#' @param item an item design matrix that is of size response categories (m) by four:
#'   * column one is category values, usually from 0 to m. Sometimes referred to as 'x',
#'     and in this case, this value times the discrimination is the category score.
#'   * column two is the delta dot parameter repeated m times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for the first response category (x = 0) tau = 0,
#'     and for m >= 2, entries are deviations from delta dot. In the dichotomous case, all items
#'     in this column are zero.
#'   * column four is the discrimination parameter ("a")
#' @param from a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   Describing the parameterisation of _item_
#' @param to a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   Describing the output parameterisation of the returned _item_ parameter matrix
#'   Note that "muraki" assumes the scaling constant D = 1.7 (to give the normal ogive metric)
#' @param D a number, giving the scaling constant. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return an m x 4 matrix of item parameters. The same dimensions as the input, _item_
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' a <- 1.5
#' m <- 3
#' itemParamX <- seq(0, m-1, 1)
#' itemParamD <- rep(myDelta, m)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, m)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myItem <- cnvrtItemParam(itemParam, from = "conquest", to = "muraki")
cnvrtItemParam <- function(item, from = "muraki", to = "conquest", D = 1) {
  # check item, check from and to
  # TODO
  myItem_t <- item
  if (from == "muraki") {
    # item discrimination
    myItem_t[, 4] <- item[, 4] * D
    # item locations ("conquest delta dots", in delta plus tau notation)
    myItem_t[, 2] <- item[, 2] * (D * item[, 4])
    # item steps ("conquest taus", in delta plus tau notation)
    # -1 takes us from easiness to difficulty parameterisation
    myItem_t[, 3] <- -1 * item[, 3] * (D * item[, 4])
  } else {
    # conquest -> Muraki
    myItem_t[, 4] <- item[, 4] / D # a = tau/D
    myItem_t[, 2] <- item[, 2] / (D * myItem_t[, 4]) # b = delta_dot/(D*a)
    myItem_t[, 3] <- -1 * item[, 3] / (D * myItem_t[, 4]) # d = -tau/(D*a)
  }
  return(myItem_t)
}

#' @title simplep
#'
#' @description returns response probabilities for each response category of an item at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param item an item design matrix that is of size response categories (m) by four:
#'   * column one is category values, usually from 0 to m. Sometimes referred to as 'x',
#'     and in this case, this value times the discrimination is the category score.
#'   * column two is the delta dot parameter repeated m times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for the first response category (x = 0) tau = 0,
#'     and for m >= 2, entries are deviations from delta dot. In the dichotomous case, all items
#'     in this column are zero.
#'   * column four is the discrimination parameter ("a")
#' @param model a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   This tells downstream functions what parameterisation has been used for the model and helps with
#'   plotting and other outputs.
#' @param D a number, giving the scaling constant. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return a k x 1 matrix of response probabilities evaluated at theta.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' a <- 1.5
#' k <- 3
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- rep(myDelta, k)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myProbs <- simplep(myTheta, itemParam)
simplep <- function(theta, item, model = "conquest", D = 1) {
  if (missing(model)) model <- "conquest"
  if (!model %in% c("muraki", "conquest")) stop("model must be muraki or conquest")
  if (!is.numeric(theta) && length(theta) != 1) stop("theta must be a scalar value")
  if (!is.matrix(item) && !ncol(item) %in% c(3, 4) && nrow(item) > 1) {
    stop("item must be a matrix with at least scores, delta dots, and taus")
  }
  if (ncol(item) == 3) {
    item <- cbind(item, rep(1, length(item[ , 1])))
    message("assuming item discrimination is constant and equal to 1")
  }
  tmp <- matrix(NA, nrow = nrow(item))
  probs <- matrix(NA, nrow = nrow(item))
  #tmp_delta_plus_tau <- 0
  for (i in seq_len(nrow(item))) {
    tmp[i , 1] <- exp(
      if (model == "muraki") {
        # Da(x*theta_m - b_m + d_m) # Muraki's GPCM
        1.7 * item[i, 4] * (item[i, 1] * theta - item[i, 2] + item[i, 3])
      } else {
        # or
        # (a*b*theta_a) - sum(delta_a+tau_a) # Adams' MMCLM
        (item[i, 4] * item[i, 1] * theta) - (sum(item[1:i, 2:3]))
      }
    )
  }
  denom <- sum(tmp[ , 1])
  for (i in seq_len(nrow(item))) {
    probs[i , 1] <- tmp[i , 1] / denom
  }
  return(probs)
}

#' @title simplef
#'
#' @description returns expected score at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param item an item design matrix that is of size response categories (k) by three. The three columns are:
#'   * column one is scoring values, usually from 0 to k.
#'   * column two is the delta dot parameter repeated k times (the average difficulty of the item)
#'   * column three is the tau (step) parameter where for k = 1, tau = 0, and for k >= 2,
#'     subsequent entries are deviations from delta dot.
#'   * column four is the discrimination paramter ("a")
#' @param model a string, either "muraki"  or "conquest" (default) (see 10.1177/0146621697211001).
#'   This tells downstream functions what parameterisation has been used for the model and helps with
#'   plotting and other outputs.
#' @param D a number, giving the scaling constant. Default is 1 (logistic metric).
#'   Other common values are D = 1.7 (to give the normal ogive metric)
#'
#' @return a double - the expected score at theta.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' k <- 3
#' a <- 1
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- rep(myDelta, k)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myExpect <- simplef(myTheta, itemParam)
simplef <- function(theta, item, model, D) {
  if (missing(model)) model <- "conquest"
  if (missing(D)) D <- 1
  expt <- 0
  probs <- simplep(theta, item, model)
  for (i in seq(probs)) {
    expt <- expt + (item[i, 1]) * probs[i, 1]
  }
  return(expt)
}

#' @title pX
#'
#' @description returns response probabilities for given response _x_ to an item.
#'
#' @param x a scalar integer - a response to an item (usually in the range 0, k-1,
#'     where k is the number of response categories).
#' @param probs a matrix returned from simplef.
#' @return a 1 x 1 matrix giving the response probability of _x_.
#' @keywords internal
#' @examples
#' myTheta <- 0
#' myDelta <- 1.5
#' k <- 3
#' a <- 1.25
#' itemParamX <- seq(0, k-1, 1)
#' itemParamD <- rep(myDelta, k)
#' itemParamT <- c(0, -0.5, 0.5)
#' itemParamA <- rep(a, k)
#' itemParam <- cbind(itemParamX, itemParamD, itemParamT, itemParamA)
#' colnames(itemParam)<- c("x", "d", "t", "a")
#' myProbs <- simplef(myTheta, itemParam)
#' myProb <- pX(0, myProbs)
pX <- function(x, probs) {
  return(probs[x+1])
}

#' @title theta_ll
#'
#' @description returns the log of the likelihood of theta, given a vector of 
#'   item responses, item parameters.
#'   Note that this is the simple ll - it is the continuing product of the 
#'   response probabilities. It includes a hack, that will nudge the raw 
#'   response to the first item to adjust for zeros and perfects.
#'   This should be updated to work with adjusted raw scores instead...
#'
#' @param theta  a scalar value of theta.
#' @param responses a vector of item responses (used in a call to `pX`).
#' @param itemParams a list of item designs (each used in a call to `simplef`). 
#'   Must be of same length as `responses`.
#'
#' @return a double, the log of the likelihood at theta..
#' @keywords internal
theta_ll <- function(theta, responses, itemParams) {
  if (length(responses) != length(itemParams)) {
    stop(
      "you must provide a vector of responses and a list with an item design for each response"
    )
  }
  # drop items where response in NA
  itemParamsT <- list()
  if (any(is.na(responses))) {
    itemParamsT <- itemParams[[!is.na(responses)]]
    itemParams <- itemParamsT
  }
  results <- list()
  tmpProbs <- list()

  # deal with all NA, zero or perfects
  if (all(is.na(responses))) responses[1] <- 0.3 # this feels SUUUUUPER hacky
  tmpRaw <- rawScore(x = responses, itemParams = itemParams)
  if (as.numeric(tmpRaw[1]) == 0)
  {
    responses[!is.na(responses)][1] <-0.3

  } else if (as.numeric(tmpRaw[1]) == as.numeric(tmpRaw[3]))
  {
    responses[!is.na(responses)][1] <- (responses[!is.na(responses)][1]-0.3)
  }

  for (i in seq(length(responses))) {
    if (is.na(responses[i]))
    {
      results[[i]] <-0
    } else
    {
      p <- simplep(theta, itemParams[[i]])
      px <- pX(responses[i], p)
      results[[i]] <- log(px)
    }
  }
  ll <- sum(unlist(results))
  return(ll)
}



#' @title makeItemList
#'
#' @description creates a list of item matrices.
#'    Each matrix represent one item's set of item parameters. The structure of the
#'    matrix is the same as used in `conquestr::simplef`
#'    (a matrix of k categories by four (category score, delta dot, tau, discrimination)).
#'
#' @param scores a data frame or matrix containing category scores for each item.
#'    If NULL, it is assumed increasing integer scoring starting at 0 is used for all items (that is, the
#'    first category is scored 0, the second category is scored 1, the \eqn{k^{th}}{kth} category is scored k-1).
#'
#'    If a data frame, column labels should be "id", "itemid", "step", "score".
#'    If a matrix, the column order should be: "id", a unique item ID for each item matched with values in `deltaDot`;
#'    "itemid", item labels for each item (or NA); "step", an indicator of which step/item category this score
#'    represents and "score" the value for the scoring parameter associated with this category.
#'    There must be one score for each category (i.e. 2 for dichotomies and one for each of k categories for
#'    polytomies).
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "score" is numeric
#'    * The original category scores (i.e., increasing integer scoring) is preserved in the rownames of the matrix.
#'
#' @param deltaDot a data frame or matrix of delta dots (average item location/difficulty for each item).
#'
#'    If a data frame, column labels should be: "id", "itemid", "delta".
#'    "itemid" should be populated with an item label or be missing for all values.
#'    If a matrix, column order should be: "id", a unique item ID for each row; "itemid", item labels for each item
#'    (or NA); "delta", a delta dot.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "delta" is numeric
#'
#' @param tau NULL if all items are dichotomies.
#'    A data frame or matrix of taus for polytomous items. Only polytomous items should be in this file.
#'    If an item ID in `deltaDot` in not in `tau` it is assumed that the item is dichotomous.
#'    The tau parameters represent the deviation from the delta dot to give the item parameters for adjacent category
#'    boundaries (e.g., delta one (
#'    \eqn{\delta_{1} = \dot{\delta} + \tau_{1}}{delta_1 = delta_dot + tau_1}
#'    ) is the boundary between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2},
#'    delta two (
#'    \eqn{\delta_{2} = \dot{\delta} + \tau_{2}}{delta_2 = delta_dot + tau_2}
#'    ) is the category boundary between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2}).
#'
#'    Where a polytomous item has k categories, there should be k-2 rows for that item in `tau`. For example,
#'    a 3-category item has categories \eqn{k_{1}}{k_1}, \eqn{k_{2}}{k_2} and \eqn{k_{3}}{k_3}.
#'    There will be one value in `tau` for this item.
#'    The value in `tau` represents the the first category boundary.
#'    (e.g., between \eqn{k_{1}}{k_1} and \eqn{k_{2}}{k_2}).
#'    The last (second in this case) category boundary is constrained to be the negative sum of the other tau values
#'    within this item (and is therefore not required in the file).
#'
#'    If a data frame, column labels should be "id", "itemid", "step", "tau".
#'    If a matrix, the column order should be: "id", a unique item ID for each item matched with values in `deltaDot`;
#'    "itemid", item labels for each item (or NA); "step", an indicator of which step/item category this threshold
#'    represents (minimum value should be 1 and maximum value should be k-1);
#'    "tau" the value for the tau parameter associated with this step.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "tau" is numeric
#'
#' @param discrim a double, a data frame, or a matrix of item (or category) discrimination parameters.
#'    When a double is provided, the value is applied to all discrimination parameters.
#'    The default is 1. Setting the value to 1.7 is one approach to re-scale to the normal ogive metric.
#'    Otherwise a data.frame or matrix defining the discrimination parameter for each response category.
#'    If a data frame, column labels should be "id", "itemid", "step", "discrim".
#'    If step is NA and there is only one entry for an item "itemid", the discrimination is
#'    assumed to be constant for all response categories with the item.
#'    This is the case for names models like the GPCM and 2PL models, and can be a short
#'    hand way of defining the discrimination without specifying all categories.
#'    When discrimination varies across scoring categories, the bock-nominal model is implied.
#'    In the case of discrimination varying across scoring categories, all categories must be
#'    defined.
#'
#'    If a data frame, or a matrix:
#'    * "id" is an integer
#'    * "itemid" is a character string
#'    * "step" is an integer
#'    * "discrim" is numeric
#'
#' @return a list.
#' @examples
#' nItems <- 10
#' myItemsDeltaDot <- data.frame(
#'   id= seq(nItems),
#'   itemid= NA,
#'   delta = runif (nItems, -4, 1) # nItems items in range -4,1
#' )
#' myItemsList <- conquestr::makeItemList(deltaDot = myItemsDeltaDot)
makeItemList <- function(scores = NULL, deltaDot, tau = NULL, discrim = 1) {
  # used for internal debugging
  isDebug <- FALSE

  # return object
  myItems <- list()

  # set up
  if (is.null(tau)) {
  tau <- data.frame(
      id = NA_integer_,
      itemid = NA_character_,
      step = NA_integer_,
      tau = NA_real_
    )
  }
  isScored <- FALSE
  hasDiscrims <- FALSE
  if (!is.null(scores)) isScored <- TRUE
  myScr <- scores
  myDlt <- deltaDot
  myTau <- tau
  if (!inherits(discrim, "numeric")) hasDiscrims <- TRUE
  myDiscim <- discrim

  itemCounter <- 1

  # coerce matrices to dfs
  if (inherits(myScr, "matrix")) {
    myScr <- data.frame(
      id = scores[, 1],
      itemid = scores[, 2],
      step = scores[, 3],
      score = scores[, 4]
    )
  }
  if (inherits(myDlt, "matrix")) {
    myDlt <- data.frame(
      id = deltaDot[, 1],
      itemid = deltaDot[, 2],
      delta = deltaDot[, 3]
    )
  }
  if (inherits(myTau, "matrix")) {
    myTau <- data.frame(
      id = tau[, 1],
      itemid = tau[, 2],
      step = tau[, 3],
      tau = tau[, 4]
    )
  }
  if (inherits(myDiscim, "matrix")) {
    myDiscim <- data.frame(
      id = discrim[, 1],
      itemid = discrim[, 2],
      step = discrim[, 3],
      score = discrim[, 4]
    )
  }

  # error checking
  if (!inherits(myDlt, "data.frame") || !inherits(myTau, "data.frame")) {
    stop("deltaDot and tau must be data frames or matricies")
  }
  if (any(!c("id", "delta") %in% names(myDlt))) {
    stop("'deltaDot' must contain the column labels 'id' and 'delta'")
  }
  if (any(!c("id", "step", "tau") %in% names(myTau))) {
    stop("'tau' must contain the column labels 'id', 'step', and 'tau'")
  }
  if (inherits(myDlt, "data.frame")) {
    if (!is.numeric(myDlt$delta)) stop("in `deltaDot`, 'delta' must be numeric")
    if (!is.integer(myDlt$id)) stop("in `deltaDot`, 'id' must be an integer")
  }
  if (inherits(myTau, "data.frame")) {
    if (!is.numeric(myTau$tau)) stop("in `tau`, 'tau' must be numeric")
    if (!is.integer(myTau$id)) stop("in `tau`, 'id' must be an integer")
    if (!is.integer(myTau$step)) stop("in `tau`, 'step' must be an integer")
    # remember that tau can be all NA, so double check this too
    if (!all(tau$id %in% myDlt$id) && !all(is.na(tau$id))) stop(
      paste0(
          "all items ids in `tau` must also exist in `deltaDot`",
          " problem with item id ",
          tau$id[which(!tau$id %in% myDlt$id)],
          " in `tau` but not in `deltaDot`"
        )
    )
  }
  if (inherits(myScr, "data.frame")) {
    if (!is.numeric(myScr$score)) stop("in `score`, 'myScr' must be numeric")
    if (!is.integer(myScr$id)) stop("in `myScr`, 'id' must be an integer")
    if (!all(myScr$id %in% myDlt$id)) stop(
      paste0(
          "all items ids in `scores` must also exist in `deltaDot`",
          " problem with item id ",
          myScr$id[which(!myScr$id %in% myDlt$id)],
          " in `scores` but not in `deltaDot`"
        )
    )
  }
  if (inherits(myDiscim, "data.frame")) {
    if (!is.numeric(myDiscim$discrim)) stop("in `myDiscim`, 'discrim' must be numeric")
    if (!is.integer(myDiscim$id)) stop("in `myDiscim`, 'id' must be an integer")
    if (!all(myDiscim$id %in% myDlt$id)) stop(
      paste0(
          "all items ids in `discrim` must also exist in `deltaDot`",
          " problem with item id ",
          myDiscim$id[which(!myDiscim$id %in% myDlt$id)],
          " in `discrim` but not in `deltaDot`"
        )
    )
  } else {
    if (!inherits(myDiscim, "numeric")) stop("Discrim must be numeric")
  }

  # need to check id is unique!
  # need to check that tau$id in deltaDot$id

  # use scores, deltas, taus, and discrims to make conquestr items list
  for (item in unique(myDlt$id)) {
    if (isDebug) print(paste0("Begin working on item: ", item))
    tName <- myDlt$itemid[myDlt$id == item]
    tmpMyDlt <- myDlt[myDlt$id == item, ]
    if (!item %in% myTau$id) { # if item is dichotomous
      if (isDebug) print(paste0("item: ", item, " is dichotomous"))
      tItm <- matrix(c(0, 0, 0, 1, 1, tmpMyDlt$delta, 0, 1), ncol = 4, byrow = TRUE)
      colnames(tItm) <- c("k", "d", "t", "a")
    } else { # item is polytomous
      tmpMyTau <- myTau[myTau$id == item , ]
      tStps <- max(tmpMyTau$step) + 2
      if (isDebug) print(paste0("item: ", item, " is polytomous and has ", tStps, " steps"))
      tIdCnstr <- -1 * sum(tmpMyTau$tau) # negative sum of taus
      tItm <- matrix(rep(c(NA, tmpMyDlt$delta, NA, 1), tStps), ncol = 4, byrow = TRUE)
      colnames(tItm) <- c("k", "d", "t", "a")
      for (i in seq(tStps)) {
        if (i == 1) {
          tItm[i, 1] <- 0
          tItm[i, 2] <- 0
          tItm[i, 3] <- 0
        } else if (i < max(tStps)) {
          tItm[i, 1] <- (i - 1)
          tItm[i, 3] <- tmpMyTau$tau[tmpMyTau$step == (i - 1)]
        } else {
          tItm[i, 1] <- (i - 1)
          tItm[i, 3] <- tIdCnstr
        }
      }
    }
    # Discrimination
    if (is.numeric(myDiscim) && myDiscim != 1) {
       tItm[, 4] <- tItm[, 4] * myDiscim
    }
    if (hasDiscrims) {
      # work out if there is one entry per item
      tmpMyDiscrim <- myDiscim[myDiscim$id == item , ]
      if (length(tmpMyDiscrim$step) == 1 && is.na(tmpMyDiscrim$step)) {
        tItm[, 4] <- tmpMyDiscrim$discrim
      } else {
        # or if there is many entries per item
        if (length(tmpMyDiscrim$discrim) != length(tItm[, 4])) {
          stop(
            paste0(
              "problem with item ", item, ": you must provide",
              " a discrimination value for each response category",
              " or provide one value for each item and ensure",
              " the step value is NA"
            )
          )
          tItm[, 4] <- tmpMyDiscrim$discrim
        }
      }
    }
    # Scores
    if (isScored) {
      # save original categories as rownames
      rownames(tItm) <-  tItm[, 1]
      # work out if there is one entry per item
      tmpMyScr <- myScr[myScr$id == item, ]
      if (isDebug) {
        print(
          paste0("applying scores to item: ", item, " the following scores are provided by the user:\n")
        )
        print(tmpMyScr)
      }
      if (length(tmpMyScr$score) == length(tItm[, 1])) {
        tItm[, 1] <- tmpMyScr$score
      } else {
        stop(
          paste0(
            "problem with item ", item, ": you must provide",
            " a score value for each response category",
          )
        )
      }
    }

    # build return object
    myItems[[itemCounter]] <- tItm
    if (!is.na(tName)) names(myItems)[itemCounter] <- tName
    itemCounter <- itemCounter + 1
  }
  return(myItems)
}


#' @title mleCalc
#'
#' @description returns the MLE for each case and asymptotic variance given a _fixed_ set of item parameters.
#'    This is done by finding the root of the distance between the observed and expected score at theta.
#'    That is, the location where O-E = 0.
#'
#' @param responses a data frame made up of rows of vectors of item responses, one for each case.
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param ... optional arguments, e.g., perfAdj.
#'
#' @return a data frame.
#' @keywords internal
#' @importFrom stats optim
mleCalc <- function(responses, itemParams, ...) {
  myMle <- list()
  for (i in seq(length(responses[ , 1]))) {
    myMle[[i]] <- list()
    myScores <- rawScore(x = responses[i , ], itemParams = itemParams)
    tmpMle <- optim(
      0, theta_ll, method = "BFGS", control = list(fnscale = -1),
      responses = responses[i , ], itemParams = itemParams, hessian = TRUE
    )
    myMle[[i]][["rawScore"]] <- myScores[[1]]
    myMle[[i]][["maxScore"]] <- myScores[[3]]
    myMle[[i]][["est"]] <- tmpMle$par
    # variance of the MLE given by Newton R is inverse of the negative Hessian
      #(i.e. the inverse of the negative of the matrix)
    #print(paste0("inv hessian for case: ", i))
    myMle[[i]]["se"] <- sqrt(solve(-tmpMle$hessian))
  }
  myMles <- matrix(unlist(myMle), ncol = 4, byrow = TRUE)
  myMles <- as.data.frame(myMles)
  names(myMles) <- c("raw_score", "max_score", "est", "se")
  return(myMles)
}

#' @title getItemMatrix
#'
#' @description generates an item matrix. Useful for
#'
#' @param responses a data frame made up of rows of vectors of item responses, one for each case.
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param ... optional arguments, e.g., perfAdj.
#'
#' @return a data frame.
#' @keywords internal
#' @importFrom stats optim
getItemMatrix <- function(responses, itemParams, ...) {
  myMle <- list()
  for (i in seq(length(responses[ , 1]))) {
    myMle[[i]] <- list()
    myScores <- rawScore(x = responses[i , ], itemParams = itemParams)
    tmpMle <- optim(
      0, theta_ll, method = "BFGS", control = list(fnscale = -1),
      responses = responses[i , ], itemParams = itemParams, hessian = TRUE
    )
    myMle[[i]][["rawScore"]] <- myScores[[1]]
    myMle[[i]][["maxScore"]] <- myScores[[3]]
    myMle[[i]][["est"]] <- tmpMle$par
    # variance of the MLE given by Newton R is inverse of the negative Hessian
      #(i.e. the inverse of the negative of the matrix)
    #print(paste0("inv hessian for case: ", i))
    myMle[[i]]["se"] <- sqrt(solve(-tmpMle$hessian))
  }
  myMles <- matrix(unlist(myMle), ncol = 4, byrow = TRUE)
  myMles <- as.data.frame(myMles)
  names(myMles) <- c("raw_score", "max_score", "est", "se")
  return(myMles)
}

#' @title writeImportXsi
#'
#' @description Writes a fixed width text file in the format required for the 
#'   ACER ConQuest command and argument `import anchor_xsi`. Can also be used
#'   for initial values, though caution should be used with the interpretation 
#'   of the argument `lconstraint` which should relate to the model of interest 
#'   ACER ConQuest
#' 
#'   Currently only works with implicit variables. Explicit variables may be added
#'   in the future. 
#'
#' @param items a list of item matrices
#' @param bmatix either the integer 1L for a unidimensional model, or
#'   a matrix, items by dimensions with 1L representing that the item in on this
#'   dimension, and a 0 otherwise.
#' @param lconstraint the identification constraint in use, one of "none", "items",
#'   or "cases".
#' @param file a path and filename to write file to disk.
#' #' @return invisibly returns path of file written to disk)
#' @seealso [conquestr::simplef()], [conquestr::genResponses()], `browseVignettes("conquestr")`
#' @examples
#'   myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
#'   myItems <- list(myItem, myItem)
#'   myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
#'   myResponses <- genResponses(abilities = rnorm(100), itemParams = myItems)
writeImportXsi <- function(items, bmatix = 1L, lconstraint = "none", file) {
  # TODO: check inputs are valid
  if(!is.list(items)) stop("'items' must be of class list")
  if(missing((file))) stop("'file' must be delcared")

  myXsiFile <- list()
  myXsiFile[["deltaDots"]] <- list()
  myXsiFile[["steps"]] <- list()
  
  for (i in length(items)) {
    # colate delta dots
    myXsiFile[["deltaDots"]][[i]] <- items[[i]][ncol(items[[i]]) , 2]
  }
  

  return(NULL)
}
