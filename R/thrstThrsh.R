#' @title thrstThrsh
#'
#' @description Generates Thurstonian Thresholds (sometimes called _gammas_) to an item.
#'   Thurstonian thresholds are the location on the trait/scale at which the cumulative probability of being in category k, or any higher category equals some probability (usually 0.5, the default).
#'   Thurstonian thresholds are considered a way of describing the difficulty of polytomously scored items and are usually the value used in visualisations like Wright maps.
#'   Thurstonian thresholds can only be calculated for items where response categories are scored such that each category can be placed in an order increasing scores (e.g., no ties as per the Ordered Partition model)
#'
#' @param myItem A matrix of parameters for a single item of the structure used in `simplef` (a matrix of k categories by three (category score, delta dot, tau)).
#' @param threshP The probability at which the threshold is calculated (defaults to the usual value of 0.5)
#' @param minTheta The lower-bound starting value of the split-half search used to find the threshold for the category.
#' @param maxTheta The upper-bound starting value of the split-half search used to find the threshold for the category.
#' @param convC The convergence criteria used to determine when the threshold has been found. The difference between `threshP` and the cumulative probability of the category and any higher category at the current value of theta (the current threshold being tested).
#' @return A k-1 by 1 matrix with Thurstonian thresholds for this item. Values are NA when the threshold cannot be calculated.
#' @examples
#' myItem<- matrix(
#'   c(
#'     0, -0.58    ,  0,     # delta+tau   thurst thresh (gamma)
#'     1, -0.58    ,  0.776, # 0.196       -1.14
#'     2, -0.58    , -0.697, # -1.277      -0.93
#'     3, -0.58    , -0.629, # -1.209      -0.64
#'     4, -0.58    ,  0.55   # -0.03        0.25
#'     ), ncol =3, byrow=TRUE
#'  )
#' thrstThrsh(myItem)
thrstThrsh<- function(myItem, threshP = 0.5, minTheta = -20, maxTheta = 20, convC = 0.00001){

  maxK<- length(myItem[,1])
  result<- matrix(NA, maxK-1, 1)

  # iter through each threshold (k-1) for this i
  for(k in seq(length(result))){
    TminTheta<- minTheta
    TmaxTheta<- maxTheta
    tCount<- 1
    cumPk<- 1
    lastMove<- ""
    while(!isTRUE(all.equal(cumPk, threshP, tolerance = convC)))
    {
      theta<- mean(c(TminTheta, TmaxTheta))
      cumPk<- 1-cumsum(simplep(theta, myItem))[k] # 1-cumsum(simplep) returns the cumulative probability of k=1...k in rows k=0...k-1 - this relies on ordered and increasing scoring
      if(cumPk > threshP) # prob too large, bring the max value inwards
      {
        TmaxTheta<- theta
        lastMove<- "max"
      } else
      if(cumPk < threshP) # prob too small, bring the min value inwards
      {
          TminTheta<- theta
          lastMove<- "min"
      }
      # if the distance between TmaxTheta and TminTheta is getting small AND the distance between cumPk and threshP is not, we need to bump out the opposite of lastMove
      if(abs(TmaxTheta-TminTheta) < convC*2 & abs(cumPk - threshP) > convC*2)
      {
        #print("check limits of search in thrstThrsh")
        if(lastMove == "")
        {
          TminTheta<-TminTheta+8
          TmaxTheta<-TmaxTheta+8
        }
        if(lastMove == "max") TminTheta<-TminTheta-8
        if(lastMove == "min") TmaxTheta<-TmaxTheta+8
      }
    }
    result[k]<- theta
  }
  return(result)
}









#
#
# library(conquestr)
#
# #myItem<- matrix(c(0, 0, 0, 1, 1, -0.2), ncol =3, byrow=TRUE)
#
# # delta dot = -0.58
# # item    thresh delta
# # 79.2.1	-1.84  0.90130586
# # 79.2.2	-1.63 -0.571606436
# # 79.2.3	-1.34 -0.504300706
# # 79.2.4	-0.45  0.674744381
#
#
#
#
# ## -2.56 -0.75  1.68  0.62  5.54
# # myItem<- matrix(c(
# #  0, 0, 0,
# #  1, 0.201, -3.464,
# #  2, 0.201, -1.654,
# #  3, 0.201, 0.77,
# #  4, 0.201, -0.283,
# #  5, 0.201, 4.632
# #  ), ncol =3, byrow=TRUE)
# # colnames(myItem)<- c("k", "d", "t")
#
#
#
# convC<- 0.00001
# maxK<- length(myItem[,1])
# result<- matrix(NA, maxK-1, 1)
# threshP<- 0.5 # what is the p at the threshold?
#
# # iter through each threshold (k-1) for this i
# for(k in seq(length(result))){
#   minTheta<- -10
#   maxTheta<- 10
#   cumPk<- 1
#   while(!isTRUE(all.equal(cumPk, threshP, tolerance = convC)))
#   {
#     print(paste0("cumPk = ", cumPk))
#     theta<- mean(c(minTheta, maxTheta))
#     cumPk<- 1-cumsum(simplep(theta, myItem))[k] # 1-cumsum(simplep) returns the cumulative probability of k=1...k in rows k=0...k-1 - this relies on ordered and increasing scoring
#     if(cumPk > threshP) # prob too large, bring the max value in
#     {
#       maxTheta<- theta
#       print(paste0("new maxTheta ", maxTheta))
#     } else
#     if (cumPk < threshP) # prob too small, bring the min value in
#     {
#       minTheta<- theta
#       print(paste0("new minTheta ", minTheta))
#     }
#   }
#   print(paste0("finished cat = ", k))
#   result[k]<- theta
# }
#
#
#
#
# # if for some reason we get it on the first try
# if(isTRUE(all.equal(cumPk, threshP, tolerance = convC)))
# {
#   result[i]<- theta
#   break
# } else
#
#   thetaStep<- maxTheta
#   initGamma<- maxTheta
#   tmpDif<- initDif
#   oldDif<- tmpDif
#   iterCount<- 1
#   while(abs(tmpDif) > convC){
#     if(iterCount > 50) break
#     # get probs at theta
#     tmpP<- simplep(thetaStart, myItem)
#
#     tmpKLow<- sum(tmpP[1:i]) #p of being in cat i or lower
#     tmpKHigh<- sum(tmpP[(i+1):maxK]) #p of being in cat i+1 or higher
#     tmpDif<- tmpKLow-tmpKHigh
#     print(
#       paste0(
#         "theta = ", round(thetaStart, 4),
#         "; p(cat ", i, " or lower) = ", round(tmpKLow, 4),
#         "; and p(cat ", i+1, " or higher) = ", round(tmpKHigh, 4),
#         "; diff between is ", round(tmpDif, 4)
#       )
#     )
#
#     thetaStep<- thetaStep*0.995 # make the step in theta a bit smaller in each step
#     if(abs(tmpDif) < abs(oldDif)) thetaStep<- thetaStep/2 # if we're getting closer, take a "bigger" smaller step
#     if(tmpDif < 0 & thetaStep > 0) thetaStep<- thetaStep*-1 #if the dif is negative we want to choose a smaller theta
#     if(tmpDif > 0 & thetaStep < 0) thetaStep<- thetaStep*-1 #if the dif is negative we want to choose a smaller theta
#     #print(paste0("thetaStep = ", thetaStep))
#     thetaStart<- thetaStart+thetaStep
#
#     oldDif<- tmpDif
#     iterCount<- iterCount+1
#   }
#   result[i , 1]<- thetaStart
#
# }
#
# result
#
# rowSums(myItem[ , 2:3])
#
