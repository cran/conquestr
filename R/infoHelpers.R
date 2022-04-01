#' @title myAbilitiesHelper
#'
#' @description helper function to return attributes of vector of abilities for use in information function and WI.
#'
#' @param myWhat A character string indicating what to check.
#' @param myAbilities A numeric vector of person abilities.
#' @return A Boolean or a double.
#' @keywords internal
myAbilitiesHelper<- function(myWhat = "ok", myAbilities = NA){
  if(myWhat == "ok"){
    if(length(myAbilities) < 2 | class(myAbilities) != "numeric"){
      return(FALSE) # 	 stop("abilities must be a numeric vector of length > 1")
    } else {
      return(TRUE)
    }
  }

  if(myWhat == "min"){
    return(min(myAbilities))
  }

  if(myWhat == "max"){
    return(max(myAbilities))
  }
}

#' @title myAbilitiesHelper
#'
#' @description helper function to return attributes of vector of item deltas for use in information function and WI.
#'
#' @param myWhat A character string indicating what to check.
#' @param myItems A numeric vector of item deltas.
#' @return A Boolean or a double.
#' @keywords internal
# helper function to return attributes of vector of items for use in information function and WI
myItemHelper<- function(myWhat = "ok", myItems = NA){
  if(myWhat == "ok"){
    if(length(myItems) < 1 | class(myItems) != "numeric"){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

#' @title itemInfoAtTheta
#'
#' @description Calculates item information at a value of theta given a set of item parameters.
#'
#' @param myItem A matrix of item parameters of the structure used in `simplef`
#' @param theta a number.
#' @examples
#' anItem<-  matrix(c(0,0,0,1,1,0), nrow = 2, byrow = TRUE)
#' itemInfoAtTheta(anItem, 0)
itemInfoAtTheta<- function(myItem, theta){
  myDebug<- FALSE
  tInfo1<- 0
  tInfo2<- 0
  if(myDebug)
  {
    print(paste("theta: ", theta))
    print(paste("my item: ", myItem))
    cat("\n")
  }
  myProbs<- simplep(theta, myItem)
  for(i in seq(myProbs))
  {
    # note that myItem[1 , 1] == 0, and therefore info in first category is always 0
    tInfo1_tmp<- (myItem[i , 1]^2 * myProbs[i])
    tInfo2_tmp<- (myItem[i , 1] * myProbs[i]) # see https://research.acer.edu.au/measurement/4/
    tInfo1<- tInfo1 + tInfo1_tmp
    tInfo2<- tInfo2 + tInfo2_tmp # accumulate info across k
  }
  tInfo2<- tInfo2^2
  info<- tInfo1 - tInfo2
  return(info)
}

#' @title testInfoAtTheta
#'
#' @description Calculates test information at a value of theta given a list of many sets of item parameters.
#'
#' @param myItems A list of matrices of item parameters of the structure used in `simplef`
#' @param theta a number.
#' @examples
#' anItem<-  matrix(c(0,0,0,1,1,0), nrow = 2, byrow = TRUE)
#' testInfoAtTheta(list(anItem), 0)
testInfoAtTheta<- function(myItems, theta){
  testInfo<- 0
  for(item in myItems)
  {
    itemInfo<- itemInfoAtTheta(item, theta)
    testInfo<- testInfo + itemInfo # accumulate info across items
  }
  return(testInfo)
}


#' @title itemInfoOverTheta
#'
#' @description Calculates item information over a rage of theta given a set of item parameters.
#'    Returns a data frame with item information at a discrete set of values of theta - useful for plotting item information functions.
#'
#' @param myItem A matrix of item parameters of the structure used in `simplef`
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @examples
#' anItem<-  matrix(c(0,0,0,1,1,0), nrow = 2, byrow = TRUE)
#' itemInfoOverTheta(anItem)
itemInfoOverTheta<- function(myItem, minTheta=-6, maxTheta=6, stepTheta=0.1){

  infoOverRange<- data.frame(
    theta = seq(minTheta, maxTheta, by = stepTheta),
    info = NA
  )

  for(i in seq(infoOverRange$theta))
  {
    infoOverRange$info[i]<- itemInfoAtTheta(myItem = myItem, theta = infoOverRange$theta[i])
  }
  return(infoOverRange)
}


#' @title testInfoOverTheta
#'
#' @description Calculates test information over a range of theta given a list of many sets of item parameters.
#'    Returns a data frame with item information at a discrete set of values of theta - useful for plotting item information functions.
#'
#' @param myItems a list of item parameters of the structure used in `simplef`
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @examples
#' anItem<-  matrix(c(0,0,0,1,1,0), nrow = 2, byrow = TRUE)
#' testInfoOverTheta(list(anItem))
testInfoOverTheta<- function(myItems, minTheta=-6, maxTheta=6, stepTheta=0.1){

  infoOverRange<- data.frame(
    theta = seq(minTheta, maxTheta, by = stepTheta),
    info = NA
  )

  for(i in seq(infoOverRange$theta))
  {
    infoOverRange$info[i]<- testInfoAtTheta(myItems = myItems, theta = infoOverRange$theta[i])
  }
  return(infoOverRange)
}


#' @title infoFunc
#'
#' @description Calculate test information function, given a vector of item difficulties and a vector of abilities.
#' Returns a data frame that can be easily plotted.
#'
#' @param myItems A vector of item deltas.
#' @param myAbilities A vector of person abilities.
#' @param myAbilities A vector of person abilities.
#' @param type A character String.
#' Should the test information be calculated empirically ("empirical" - default) or analytically using moments of distribution ("approx").
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @return A data frame.
#' @examples
#' myInfo<- infoFunc(runif(10, -2, 3), rnorm(1000, 0, 1))
infoFunc<- function(myItems, myAbilities, type = "empirical", minTheta=NA, maxTheta=NA, stepTheta=NA){
  # error checking
  if(!myAbilitiesHelper("ok", myAbilities)) stop("abilities must be a numeric vector of length > 1")
  if(!myItemHelper("ok", myItems)) stop("items must be a numeric vector of length > 0")
  if(type != "empirical" & type != "approx") stop("type must be one of `emprical` or `approx`")

  # set default arguments if not passed in
  if(missing(minTheta)) minTheta<- myAbilitiesHelper("min", myAbilities)-1
  if(missing(maxTheta)) maxTheta<- myAbilitiesHelper("max", myAbilities)+1
  if(missing(stepTheta)) stepTheta<- 0.01

  # constants
  kappa<- 1.6
  # kappa<- pi/sqrt(3) # alternate/old version of constant

  # set up results object
  myThtRng<- seq(minTheta, maxTheta, by = stepTheta)
  myResults<- data.frame(
    theta = myThtRng,
    info  = NA
  )
  #
  if(type == "empirical"){
    # data structure for intermediate results
    myProbsMat<- matrix(NA, nrow = length(myThtRng), ncol = length(myItems))
    myInfoMat<-  matrix(NA, nrow = length(myThtRng), ncol = length(myItems))
    for(i in seq_along(myThtRng)){
      for(j in seq_along(1:length(myItems))){
        myProbsMat[i,j]<- (exp(myThtRng[i]-myItems[j]))/(1+exp(myThtRng[i]-myItems[j])) # probability of case at myThetaRange[i] endorsing item of difficulty item[j]
      }
    }
    myInfoMat<- myProbsMat * (1-myProbsMat) # see 1.3 in Adams and Cloney
    myResults$info<- rowSums(myInfoMat)
    return(myResults)
  }
  if(type == "approx"){
    # data structure for intermediate results
    myApproxProbsMat<- matrix(NA, nrow = length(myThtRng), ncol = length(myItems))
    for(i in seq_along(myThtRng)){
      for(j in seq_along(1:length(myItems))){
        myApproxProbsMat[i,j]<- (1/(2*pi)) * (sqrt((2*pi)/(kappa^2))) * exp(-(myThtRng[i]-myItems[j])^2/(2*(kappa^2)))
      }
    }
    myResults$info<- rowSums(myApproxProbsMat)
    return(myResults)
  }

}

#' @title informationWrightMap
#'
#' @description Plots test information function, relative to ability density, and item locations.
#'
#' @param myItems A vector of item deltas.
#' @param myAbilities A vector of person abilities.
#' @param type A character String.
#' Should the test information be calculated empirically ("empirical" - default) or analytically using moments of distribution ("approx").
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @param scaleInfo A scaling factor to apply to the plot of tesr information.
#' Because ability distribution is a PDF with area one, and a test information function has area L, this can make the plot more interpretable. Defaults to 1.
#' @return A ggplot2 object.
#' @examples
#' myInfoPlot<- informationWrightMap(runif(10, -2, 3), rnorm(1000, 0, 1), minTheta=-10, maxTheta=10)
informationWrightMap<- function(myItems, myAbilities, type = "empirical", minTheta=NA, maxTheta=NA, stepTheta=NA, scaleInfo=1){
  # error checking
  if(!myAbilitiesHelper("ok", myAbilities)) stop("abilities must be a numeric vector of length > 1")
  if(!myItemHelper("ok", myItems)) stop("myItems must be a numeric vector of length > 0")
  if(type != "empirical" & type != "approx") stop("type must be one of `emprical` or `approx`")

  # set default arguments if not passed in
  if(missing(minTheta)) minTheta<- myAbilitiesHelper("min", myAbilities)-1
  if(missing(maxTheta)) maxTheta<- myAbilitiesHelper("max", myAbilities)+1
  if(missing(stepTheta)) stepTheta<- 0.01

  # create DF with info function
  infoDf<- infoFunc(myItems, myAbilities, type, minTheta, maxTheta, stepTheta)

  casesDf<- data.frame(
    theta = myAbilities
  )

  myItemsDf<- data.frame(
    delta = myItems,
    myY = 0
  )

  myPlot<- ggplot2::ggplot(casesDf, ggplot2::aes(x = .data$theta) ) +
    # ABOVE: ability dist
    ggplot2::geom_density(fill="#717aad" ) + #ggplot2::aes(y = ..density..), fill="#717aad" ) +
    ggplot2::annotate("text", label = "ability distribution", x = minTheta+0.05, y = +0.2, hjust = 0, size = 4, colour = "#717aad") +
    # BELOW: info function
    ggplot2::geom_line(data = infoDf, ggplot2::aes(x = .data$theta, y = -(.data$info/scaleInfo))) +
    {if(type == "empirical") ggplot2::annotate("text", label = "information function", x = minTheta+0.05, y = -0.2, hjust = 0, size = 4, colour = "black")} +
    {if(type == "approx") ggplot2::annotate("text", label = "approximation of information function", x = minTheta+0.05, y = -0.2, hjust = 0, size = 4, colour = "black")} +
    # Theming
    ggplot2::theme_bw() +
    ggplot2::geom_point(data = myItemsDf, ggplot2::aes(x = .data$delta, y = .data$myY)) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  # coord_flip()

  return(myPlot)
}

#' @title informationWrightMap2
#'
#' @description Plots test information function, relative to ability density, and item locations (delta dots).
#'
#' @param myItems A list of sets of item parameters.
#' @param myAbilities A vector of person abilities.
#' @param minTheta The smallest value of ability PDF to plot.
#' @param maxTheta The largest value of ability PDF to plot.
#' @param stepTheta The increment to iterate over the ability PDF. Defaults to 0.01.
#' @param scaleInfo A scaling factor to apply to the plot of tesr information.
#'     Because ability distribution is a PDF with area one, and a test information function has area L, 
#'     this can make the plot more interpretable. Defaults to 1.
#' @return A ggplot2 object.
#' @examples
#' myDetaDot <- data.frame(
#'   id = 1:10,
#'   itemid = paste0("item_", letters[1:10]),
#'   delta = runif(n = 10, min = -3, max = 2)
#' )
#' myTau<- data.frame(
#'   id = c(rep(1:2, each = 1), rep (3, 2)), 
#'   itemid = paste0("item_", c(rep(letters[1:2], each = 1), rep(letters[3], 2))),
#'   step = c(1, 1, 1, 2),
#'   tau = c(-1.2, -0.3, -0.2, -0.1)
#' )
#' myItemParams<- makeItemList(myDetaDot, myTau)
#' myAbil <- rnorm(1000, 0, 1)
#' myInfoPlot <- informationWrightMap2(myItems= myItemParams, myAbilities = myAbil)
informationWrightMap2 <- function(myItems, myAbilities, minTheta=-6, maxTheta=6, stepTheta=0.1, scaleInfo=1){
  # error checking

  # create DF with info function
  infoDf<- testInfoOverTheta(myItems = myItems, minTheta=minTheta, maxTheta=maxTheta, stepTheta=stepTheta)

  # create DF with abilities
  casesDf<- data.frame(
    theta = myAbilities
  )

  # create DF with deltaDots
  iter<- 1
  for(item in myItems){
    if(iter == 1)
    {
      deltaDots<-  item[2,2] # any value in column 2 will work
      iter<- iter +1
    } else
    {
      deltaDots<- c(deltaDots, item[2,2])
      iter<- iter +1
    }
  }
  myItemsDf<- data.frame(
    delta = deltaDots,
    myY = 0
  )

  myPlot<- ggplot2::ggplot(casesDf, ggplot2::aes(x = .data$theta) ) +
    # ABOVE: ability dist
    ggplot2::geom_density(fill="#717aad" ) + #ggplot2::aes(y = ..density..), fill="#717aad" ) +
    ggplot2::annotate("text", label = "ability distribution", x = minTheta+0.05, y = +0.2, hjust = 0, size = 4, colour = "#717aad") +
    # BELOW: info function
    ggplot2::geom_line(data = infoDf, ggplot2::aes(x = .data$theta, y = -(.data$info/scaleInfo))) +
    ggplot2::annotate("text", label = "information function", x = minTheta+0.05, y = -0.2, hjust = 0, size = 4, colour = "black") +
    # Theming
    ggplot2::theme_bw() +
    ggplot2::geom_point(data = myItemsDf, ggplot2::aes(x = .data$delta, y = .data$myY)) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  # coord_flip()

  return(myPlot)
}




#' @title infoWI
#'
#' @description Calculates an index representing the product of a test information function and an ability distribution.
#'
#' @param myItems A vector of item deltas.
#' @param myAbilities A vector of person abilities.
#' @param type A character String.
#' Should the test information be calculated empirically ("empirical" - default) or analytically using moments of distribution ("approx").
#' @return A double.
#' @examples
#' infoWIOut<- infoWI(runif(10, -2, 3), rnorm(1000, 0, 1))
infoWI<- function(myItems, myAbilities, type = "empirical"){
  # error checking
  if(!myAbilitiesHelper("ok", myItems)) stop("abilities must be a numeric vector of length > 1")
  if(!myItemHelper("ok", myItems)) stop("items must be a numeric vector of length > 0")
  if(type != "empirical" & type != "approx") stop("type must be one of `emprical` or `approx`")

  # set default arguments if not passed in
  minTheta<- myAbilitiesHelper("min", myItems)
  maxTheta<- myAbilitiesHelper("max", myItems)


  # constants
  kappa<- 1.6
  # kappa<- pi/sqrt(3)

  # NOTE this reproduces much of what is in infoFunc, but instead of calculating over a range of theta (from min to max, by step), it iterates over the observed values in the vector `abilities`
  # set up results object
  myResults<- data.frame(
    theta = myItems,
    info  = NA
  )
  #
  if(type == "empirical"){
    # data structure for intermediate results
    myProbsMat<- matrix(NA, nrow = length(myItems), ncol = length(myItems))
    myInfoMat<-  matrix(NA, nrow = length(myItems), ncol = length(myItems))
    for(i in seq_along(myItems)){
      for(j in seq_along(1:length(myItems))){
        myProbsMat[i,j]<- (exp(myItems[i]-myItems[j]))/(1+exp(myItems[i]-myItems[j])) # probability of case at myThetaRange[i] endorsing item of difficulty item[j]
      }
    }
    myInfoMat<- myProbsMat * (1-myProbsMat) # see 1.3 in Adams and Cloney
    myInfoMatSum<- rowSums(myInfoMat)
    myResults$info<- myInfoMat

    myWI<- sum(myResults$info)/length(myItems)
    return(myWI)

  }
  if(type == "approx"){
    # data structure for intermediate results
    myVar<- var(myAbilities)
    kapSig<- kappa^2 + myVar
    tmpRes<- matrix(NA, ncol = 1, nrow = length(myItems))
    for(i in seq_along(tmpRes))
    {
      tmpRes[i]<- exp(-1/2*(myItems[i]^2/kapSig))
    }
    # old
    #myApproxProbsMat<- matrix(NA, nrow = length(myItems), ncol = length(myItems))
    #for(i in seq_along(myAbilities)){
    #  for(j in seq_along(1:length(myItems))){
    #    myApproxProbsMat[i,j]<- (1/(2*pi)) * (sqrt((2*pi)/(kappa^2))) * exp(-(myItems[i]-myItems[j])^2/(2*(kappa^2)))
    #  }
    #}
    #myResults$info<- rowSums(myApproxProbsMat)

    #print(paste0("myVar: ", myVar))
    #print(paste0("kapSig: ", kapSig))
    #print(paste0("tmpRes: ", tmpRes))

    myWI<- 1/(sqrt(2*pi*kapSig))*colSums(tmpRes)
    return(myWI)

  }


}