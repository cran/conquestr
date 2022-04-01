#' @title genResponses
#'
#' @description Generates responses to i items for n cases given some item parameters, (true) person abilities, and other inputs.
#'
#' @param abilities A matrix of person abilities. One column per dimension. Rows are cases.
#' @param itemParams A list of item params of the structure used in `simplef`
#'    (a matrix of k categories by three (category score, delta dot, tau)).
#'    See conquestr::makeItemList for a helper to generate this list.
#' @param BMatrix A simplified B-matrix mapping dimensions (columns) to items (rows).
#'    Or the integer "1" if items are dichotomous and ability is uni-dimensional.
#' @param discrim A vector of item discrimination paramters,
#'      or an integer where all items have constant discrimination.
#' @param mcarP A double indicating the proportion of missing data
#'     under the MCAR assumption.
#' @param perturbP A list, where each element of the list contains a data frame
#'     referring to an item.
#'     Each data frame is either a 1 * 4 data frame describing the general
#'     perturbation to apply to the probabilities for that item
#'     (currently developed) or a pair of vectors mapping theta to
#'     probabilities (not developed).
#'    In the first case - the data frame is in this order:
#'         - "item": item number (int),
#'         - "type": the type of perturbation to apply (string, "flat", or "steep"),
#'         - "pivot": probability pivot point around which the
#'            perturbation is applied (double in{0 < x < 1}),
#'         - "factor": magnitude of the perturbation
#'            (double - when in {0 < x < 100} probs remain positively
#'            correlated with theta (0 = no perturbation, 100 = maximum perturbation)
#'            when in{100 < x < Inf} probs are negatively correlated with theta).
#' @return A matrix.
#' @examples
#' \dontrun{
#' myResponses<- genResponses(rnorm(1000, 0, 1), runif(10, -2, 3)) # TODO - update this
#' }
#' @importFrom stats runif
genResponses <- function(abilities, itemParams, BMatrix = 1, discrim = 1, mcarP = 0, perturbP = NULL) {
  # TODO: map items to dimensions. in the first case items are on 1 dim only.
  myAbilitiesT <- as.matrix(abilities)
  myNCases <- length(na.omit(myAbilitiesT[ , 1])) # in unidim case myNCases could be a vector
  myNItems <- length(itemParams)
  isMultDim <- FALSE
  if (is.matrix(BMatrix) & ncol(as.matrix(BMatrix)) > 1) {
      isMultDim <- TRUE
      if (!all(rowSums(BMatrix) == 1)) stop("only between-item designs currently allowed")
  #   myAbT<- matrix(NA, nrow = myNCases, ncol = myNItems)
  #   for(i in 1:myNCases){
  #     for(j in 1:myNItems){
  #       myAbT[i , j]<-  myAbilitiesT[ i , ] %*% myBMatrix[ j ,  ]
  #     }
  #   }
  # } else {
  #   myAbT<- as.vector(myAbilitiesT) * matrix(1, myNCases, myNItems) # temp matrix
  }
  #
  # create a temp response matrix, n cases by i items
  # only works in unidim case
  myResponses <- matrix(NA, myNCases, myNItems)
  for (case in seq(myNCases))
  {
    for (item in seq(myNItems))
    {
      if (isMultDim){
        thisDim <- which(BMatrix[ item, ] == 1)
      } else {
        thisDim <- 1
      }
      resp <- itemResp(myAbilitiesT[case , thisDim], itemParams[[item]])
      myResponses[case, item] <- resp
     # print(paste0("case: ", case, " item: ", item, " response:", resp, " theta: ", myAbilitiesT[case , 1] )) # debug
    }
  }

  #
  if (mcarP > 0) {
    #MCAR
    myRand2 <- matrix(runif(myNCases*myNItems), myNCases, myNItems) # used for mcarP
    for(r in 1:nrow(myResponses)){
      for(c in 1:ncol(myResponses)){
        if(myRand2[r,c] > (1 - mcarP)){
          myResponses[r,c]<- NA
        }
      }
    }
  }

  return(myResponses)
  #
  # # myDifT<- t(myItems * matrix(1, myNItems, myNCases)) # temp matrix
  # # if(length(myDiscrim) == 1) myDiscrim<- rep(myDiscrim, myNItems)
  # # myDiscrimT<- t(myDiscrim * matrix(1, myNItems, myNCases)) # temp matrix
  # #
  # # myProbs<- exp(myDiscrimT*(myAbT - myDifT))/(1+exp(myDiscrimT*(myAbT - myDifT)))
  # # myRand<- matrix(runif(myNCases*myNItems), myNCases, myNItems)
  # # myResponses<- ifelse(myProbs > myRand, 1, 0)
  #
  # # if(!is.null(perturbP)) myProbs<- perturbProbs(myProbs, perturbP)
  # # if(!is.null(perturbP)) myItemsU<- perturbDelta(myAbilities, myItems, myProbs)
  #
  # myRand2<- matrix(runif(myNCases*myNItems), myNCases, myNItems) # used for mcarP
  # if(mcarP > 0){
  #   #MCAR
  #   for(r in 1:nrow(myResponses)){
  #     for(c in 1:ncol(myResponses)){
  #       if(myRand2[r,c] > (1 - mcarP)){
  #         myResponses[r,c]<- NA
  #       }
  #     }
  #   }
  # }
  # #return(myResponses)
  # myGenResults<- list()
  # myGenResults[["myResponses"]]<- myResponses
  # # myGenResults[["myProbs"]]<- myProbs
  # myGenResults[["myAbilities"]]<- myAbilitiesT
  # myGenResults[["myItems"]]<- myItems
  # return(myGenResults)
}



#' @title perturbProbs
#'
#' @description Perturbs response probabilities.
#'
#' @param myProbs A matrix of response probabilities from n persons * i items.
#' @param perturbP A matrix of response probabilities from n persons * i items.
#' @return A matrix.
#' @keywords internal
#' @examples
#' myExProbs<- matrix(rnorm(100, 0, 1), 10, 10)
#' myExPerturb<- list(data.frame(item = 1, type = "flat", pivot = 0.5, factor = 25))
#' myProbs<- perturbProbs(myProbs = myExProbs, perturbP = myExPerturb)
#' @importFrom stats runif
perturbProbs<- function(myProbs, perturbP){
  if(!class(perturbP) %in% "list") stop("perturbP must be of type list")
  # which items to work on
  for(i in seq_len(length(perturbP))){
    tmpP<- perturbP[[i]]
    if(!class(tmpP) %in% "data.frame") stop("elements of perturbP must be of type data.frame")
    if(all(!names(tmpP) == c("item", "type", "pivot", "factor"))) stop("elements of perturbP must have vector names item, type, pivot, factor")
    # (prob + flat|steep *((pivot-prob)/100)*factor)
    myItem<- tmpP$item
    myType<- 1
    if(tmpP$type == "steep") myType<- myType*-1
    myPivot<- tmpP$pivot
    myFactor<- tmpP$factor
    for(j in seq_along(myProbs[ , myItem])){
      tmpProb<- myProbs[ j , myItem ] + myType *((myPivot-myProbs[ j , myItem ])/100)*myFactor
      if(tmpProb < 0) tmpProb<- 0 + runif(1, 0, 0.01)
      if(tmpProb > 1) tmpProb<- 1 - runif(1, 0, 0.01)
      myProbs[ j , myItem ]<- tmpProb
    }

  }
  return(myProbs)
}


#' @title itemResp
#'
#' @description Returns a probabilistic item response, given one ability and one item (a matrix k categories by three (categor score, delta dot, tau).
#'
#' @param myAblty A numeric ability.
#' @param myItem A single item (a matrix k categories by three (categor score, delta dot, tau).
#' @return A numeric score, usually an integer in the range 0, 1, 2, ..., k-1.
#' @keywords internal
#' @examples
#' myItem<- matrix(c(0, 0, 0, 1, 1, 0), ncol = 3, byrow = TRUE)
#' itemResp(0, myItem)
itemResp<- function(myAblty, myItem){
  myRand<- runif(1)
  myRspP<- simplep(myAblty, myItem)
  myCdf<- cumsum(myRspP)
  # for(prob in myCdf){
  #   if(myRand<prob)
  # }
  return(min(which(myRand<myCdf))-1)
}

#' @title perturbDelta
#'
#' @description When probabilities are perturbed, the item difficulties may need to be updated to be location where p = 0.5
#'
#' @param myAbilities A matrix of perspon abilities.
#' @param myItems A vector of item deltas.
#' @param myProbs A matrix of response probabilities from n persons * i items.
#' @return A matrix.
#' @keywords internal
#' @examples
#' myAbilities<- rnorm(1000, 0, 1)
#' myItems<- runif(10, -2, 3)
#' myExProbs<- matrix(rnorm(100, 0, 1), 10, 10)
#' myExPerturb<- list(data.frame(item = 1, type = "flat", pivot = 0.5, factor = 25))
#' myProbs<- perturbProbs(myProbs = myExProbs, perturbP = myExPerturb)
#' myPerturbDelta<- perturbDelta(myAbilities = myAbilities, myItems = myItems, myProbs = myProbs)
perturbDelta<- function(myAbilities, myItems, myProbs){
  return(TRUE)
}



# myTestResponses<- genResponses(rnorm(1000, 0, 1), runif(10, -2, 3))
# plot(myTestResponses[["myAbilities"]], myTestResponses[["myProbs"]][ , 1])
# myTestResponsesPer<- genResponses(myTestResponses[["myAbilities"]], myTestResponses[["myItems"]], perturbP = list())


#' @title rawScore
#'
#' @description returns the adjusted (for zero and perfects) and unadjusted raw scores as well as the maximum score.
#'
#' @param x a vector of scored item responses. Order of item responses is the same as the order of `itemParams`, missing item responses are scored NA.
#' @param itemParams A list of item params of the structure used in `simplef` (a matrix of k categories by three (category score, delta dot, tau)). See conquestr::makeItemList for a helper to generate this list.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#' @return a list containing three elements: (1) raw score (unadjusted), (2) raw score (adjusted), (3) maximum score.
#' @keywords internal
rawScore<- function(x, itemParams, perfAdjust = 0.3){
  myResults<- list()
  if(perfAdjust < 0 | class(perfAdjust) != "numeric") stop("perfAdjust must be numeric and greater than 0")

  # calc raw score
  myRaw<- sum(x, na.rm = TRUE)

  # calc max score
  tmpItemParams<- itemParams[!is.na(x)] # keep the item params with a valid response
  countItems<- length(x[!is.na(x)]) # how many items were responded to?
  if(length(countItems) > 0)
  {
    maxScore<- 0
    for(i in seq(countItems))
    {
      tmp<- max(tmpItemParams[[i]][ , 1]) # this is the max category for this item - TODO: when we set up for 2PL multiply by scores
      maxScore<- maxScore + tmp
    }
  } else
  {
    maxScore<- NA # no items responded to
  }

  # calc adjusted raw score
  myRawA<- myRaw
  if(perfAdjust > 0){
    if(myRaw == 0) myRawA<- myRaw + perfAdjust
    if(myRaw == maxScore) myRawA<- myRaw - perfAdjust
  }

  myResults[["raw score unadjusted"]]<- myRaw
  myResults[["raw score adjusted"]]<- myRawA
  myResults[["highest score possible"]]<- maxScore

  return(myResults)
}

#' @title thetaScore
#'
#' @description Return score (O-E) at theta. Used in estimation of theta by minimising _raw score - expected score_ at theta
#'
#' @param theta a scalar value of theta.
#' @param responses a vector of item responses for one case (can contain NA). Responses are in the same order as `items`
#' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#' @param perfAdjust the correction factor for zeros and perfects to be add/subtracted from the raw score.
#'
#' @return a double, the score (O-E) at theta.
#' @keywords internal
thetaScore<- function(theta, responses, itemParams, perfAdjust = 0.3){
  tmpRaw<- conquestr::rawScore(responses, itemParams, perfAdjust = perfAdjust)[[2]] # observed
  tmpExp<- list()
  for(i in seq(itemParams)){
    if(is.na(responses[i])){
      tmpExp[[i]]<- NA
      next
    }
    tmpExp[[i]]<- simplef(theta, itemParams[[i]])
  }
  tmpExpSum<- sum(unlist(tmpExp), na.rm = TRUE) # expected
  return(tmpRaw - tmpExpSum)
}

#' @title simplep
#'
#' @description returns response probabilities for each reponse category of an item at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param params an item design matrix that is of size response categories (k) by three. The three columns are:
#'   * column one is scoring values, usually from 0 to k.
#'   * column two is the delta dot parameter repeated k times
#'   * column three is the tau parameter where for k = 1, tau = 0, and for k > 2, subsequent entries are deviations from delta dot.
#'
#' @return a k x 1 matrix of response probabilities evaluated at theta.
#' @keywords internal
#' @examples
#' myTheta<- 0
#' myDelta<- 1.5
#' x<- 2
#' k <- 3
#' itemParamX<- seq(0, k-1, 1)
#' itemParamD<- rep(myDelta, k)
#' itemParamT<- c(0, -0.5, 0.5)
#' itemParam<- cbind(itemParamX, itemParamD, itemParamT)
#' colnames(itemParam)<- c("x", "d", "t")
#' myProbs<- simplep(myTheta, itemParam)
simplep<- function(theta, params){
  tmp<- matrix(NA, nrow = nrow(params))
  probs<- matrix(NA, nrow = nrow(params))
  for(i in seq(nrow(params)))
  {
    tmp[i , 1]<- exp(params[i, 1]*theta-sum(params[1:i, 2:3]))
  }
  denom<- sum(tmp[ , 1])
  for(i in seq(nrow(params)))
  {
    probs[i , 1]<- tmp[i , 1]/denom
  }
  return(probs)
}

#' @title simplef
#'
#' @description returns expected score at a given value of theta.
#'
#' @param theta a scalar value of theta.
#' @param params an item design matrix that is of size response categories (k) by three. The three columns are:
#'   * column one is scoring values, usually from 0 to k.
#'   * column two is the delta dot parameter repeated k times
#'   * column three is the tau parameter where for k = 1, tau = 0, and for k > 2, subsequent entries are deviations from delta dot.
#'
#' @return a double - the expected score at theta.
#' @keywords internal
#' @examples
#' myTheta<- 0
#' myDelta<- 1.5
#' x<- 2
#' k <- 3
#' itemParamX<- seq(0, k-1, 1)
#' itemParamD<- rep(myDelta, k)
#' itemParamT<- c(0, -0.5, 0.5)
#' itemParam<- cbind(itemParamX, itemParamD, itemParamT)
#' colnames(itemParam)<- c("x", "d", "t")
#' myExpect<- simplef(myTheta, itemParam)
simplef<- function(theta, params){
  expt<- 0
  probs<- simplep(theta, params)
  for(i in seq(probs))
  {
    expt<- expt + (i-1) * probs[i, 1]
  }
  return(expt)
}

#' @title pX
#'
#' @description returns response probabilities for given response _x_ to an item.
#'
#' @param x a scalar integer - a response to an item (usually in the range 0, k-1, where k is the number of response categories).
#' @param probs a matrix returned from simplef.
#'
#' @return a 1 x 1 matrix giving the response probability of _x_.
#' @keywords internal
#' @examples
#' myTheta<- 0
#' myDelta<- 1.5
#' x<- 2
#' k <- 3
#' itemParamX<- seq(0, k-1, 1)
#' itemParamD<- rep(myDelta, k)
#' itemParamT<- c(0, -0.5, 0.5)
#' itemParam<- cbind(itemParamX, itemParamD, itemParamT)
#' colnames(itemParam)<- c("x", "d", "t")
#' myProbs<- simplef(myTheta, itemParam)
#' myProb<- pX(0, myProbs)
pX<- function(x, probs){
  return(probs[x+1])
}

#' @title theta_ll
#'
#' @description returns the log of the likelihood of theta, given a vector of item responses, item parameters.
#'     Note that this is the simple ll - it is the continuing product of the response probabilities. It
#'     includes a hack, that will nudge the raw response to the first item to adjust for zeros and perfects.
#'     This should be updated to work with adjusted raw scores instead...
#'
#' @param theta  a scalar value of theta.
#' @param responses a vector of item responses (used in a call to `pX`).
#' @param params a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
#'
#' @return a double, the log of the likelihood at theta..
#' @keywords internal
theta_ll<- function(theta, responses, itemParams){
  if(length(responses) != length(itemParams)) stop("you must provide a vector of responses and a list with an item design for each response")
  results<- list()
  tmpProbs<- list()

  # deal with all NA, zero or perfects
  if(all(is.na(responses))) responses[1]<- 0.3 # this feels SUUUUUPER hacky
  tmpRaw<- rawScore(x = responses, itemParams = itemParams)
  if(as.numeric(tmpRaw[1]) == 0)
  {
    responses[!is.na(responses)][1]<-0.3

  } else if(as.numeric(tmpRaw[1]) == as.numeric(tmpRaw[3]))
  {
    responses[!is.na(responses)][1]<- (responses[!is.na(responses)][1]-0.3)
  }

  for(i in seq(length(responses))){
    if(is.na(responses[i]))
    {
      results[[i]]<-0
    } else
    {
      p<- simplep(theta, itemParams[[i]])
      px<- pX(responses[i], p)
      results[[i]]<- log(px)
    }
  }
  ll<- sum(unlist(results))
  return(ll)
}



#' @title makeItemList
#'
#' @description creates a list of matrices. Each matrix represent item parameters of the structure used in `simplef` (a matrix of k categories by three (category score, delta dot, tau)).
#'
#' @param deltaDot a data frame or matrix of delta dots. If a data frame, column labels should be: "id", "itemid", "delta".
#'     "itemid" should be populated with an item label or be missing for all values.
#'     If a matrix, column order should be: "id", a unique item ID for each row; "itemid", item labels for each item (or NA); "delta", a delta dot.
#' @param tau a data frame or matrix of taus for polytomous items. Only polytomous items should be in this file.
#'     If an item ID in `deltaDot` in not in `tau` it is assumed that the item is dichotomous.
#'     Where a polytomous item has k categories, there should be k-2 rows for that item in `tau`. For example, a 3-category item has categories k_1, k_2, and k_3. There will be one value in `tau` for this item.
#'     The tau parameters represent the deviation from the delta dot to give the item parameters for adjacent category boundaries (e.g., k_1 and k_2, and k_2 and k_3).
#'     The value in `tau` represents the the first category boundary (e.g., k_1 and k_2). The second category boundary is constrained to be the negative sum of the other values of tau for that item.
#'     If a data frame, column labels should be "id", "itemid", "step", "tau".
#'     If a matrix, the column order should be: "id", a unique item ID for each item matched with values in `deltaDot`; "itemid", item labels for each item (or NA);
#'     "step", an indicator of which step/item category this threshold represents (minimum value should be 1 and maximum value should be k-1);
#'     "tau" the value for the tau parameter associated with this step.
#' @param scores a data frame of item response category scores. Not currently used - items are assumed to be scored as per the values in `tau$step`.
#'
#' @return a list.
#' @examples
#' nItems<- 10
#' myItemsDeltaDot<- data.frame(
#'   id= seq(nItems),
#'   itemId= NA,
#'   delta = runif(nItems, -4, 1) # nItems items in range -4,1
#' )
#' myItemsTau<- data.frame(
#'   id = NA,
#'   itemId = NA,
#'   step = NA,
#'   tau = NA # this is only needed because the function is...in progress
#' )
#' myAbilities<- rnorm(100, 1, 2) # 100 person, offset from items - poor targeting (?)
#' myItemsList<- conquestr::makeItemList(deltaDot = myItemsDeltaDot, tau = myItemsTau)
makeItemList<- function(deltaDot, tau, scores = NULL){
  # get deltas
  myDlt<- deltaDot
  # get taus
  myTau<- tau
  # error checking
  if(!"data.frame" %in% class(myDlt) | !"data.frame" %in% class(tau))
  {
    stop("For now deltaDot and tau must be data frames - sorry about that. I promsise to build matrix support soon")
  }
  if(any(!c("id", "delta") %in% names(myDlt))) stop("'deltaDot' must contain the column labels 'id' and 'delta'")
  if(any(!c("id", "step", "tau") %in% names(tau))) stop("'tau' must contain the column labels 'id', 'step', and 'tau'")
  # need to check id is unique!
  # need to check that tau$id in deltaDot$id

  # use deltas and taus to make conquestr items list
  myItems<- list()
  for(item in unique(myDlt$id))
  {
    tName<- myDlt$itemid[myDlt$id == item]
    tmpMyDlt<- myDlt[myDlt$id == item , ]
    if(!item %in% myTau$id) # if item is dichotomous
    {
      tItm<- matrix(c(0, 0, 0, 1, tmpMyDlt$delta, 0), ncol = 3, byrow = TRUE)
      colnames(tItm)<- c("k", "d", "t")
    } else
    { # item is polytomous
      tmpMyTau<- myTau[myTau$id == item , ]
      tStps<- max(tmpMyTau$step)+2
      # print(item); print(tmpMyTau); print(tStps)
      tIdCnstr<- -1*sum(tmpMyTau$tau) # negative sum of taus
      tItm<- matrix(rep(c(NA, tmpMyDlt$delta, NA), tStps), ncol = 3, byrow = TRUE) # this is always a tStps * 3 matrix
      colnames(tItm)<- c("k", "d", "t")
      for(i in seq(tStps))
      {
        if(i == 1)
        {
          tItm[i,1]<- 0
          tItm[i,2]<- 0
          tItm[i,3]<- 0
        } else
        if(i < max(tStps))
        {
          tItm[i,1]<- (i-1)
          tItm[i,3]<- tmpMyTau$tau[tmpMyTau$step == (i-1)]
        } else
        {
          tItm[i,1]<- (i-1)
          tItm[i,3]<- tIdCnstr
        }
      }
    }
    myItems[[item]]<- tItm
    names(myItems)[length(myItems)]<- tName
  }
  return(myItems)
}


##' @title mleCalc
##'
##' @description returns the MLE for each case and asymptotic variance given a _fixed_ set of item parameters.
##'    This is done by finding the root of the distance between the observed and expected score at theta.
##'    That is, the location where O-E = 0.
##'
##' @param responses a data frame made up of rows of vectors of item responses, one for each case.
##' @param itemParams a list of item designs (each used in a call to `simplef`). Must be of same length as `responses`.
##' @param ... optional arguments, e.g., perfAdj.
##'
##' @return a data frame.
##' @keywords internal
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
      #(i.e. the inverse of the negative of the information matrix)
    #print(paste0("inv hessian for case: ", i))
    myMle[[i]]["se"] <- sqrt(solve(-tmpMle$hessian))
  }
  myMles <- matrix(unlist(myMle), ncol = 4, byrow = TRUE)
  myMles <- as.data.frame(myMles)
  names(myMles) <- c("raw_score", "max_score", "est", "se")
  return(myMles)
}
