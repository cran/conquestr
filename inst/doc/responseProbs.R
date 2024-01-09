## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----slmItem------------------------------------------------------------------
library(conquestr)
myItem <- matrix(
  c(
    0, 0, 0, 1,
    1, 1, 0, 1
  ), ncol = 4, byrow = TRUE
)
colnames(myItem) <- c("x", "d", "t", "a")
print(myItem)

## ----slmProbs-----------------------------------------------------------------
myProbs <- simplep(0.5, myItem)
print(myProbs)

## ----slmICC-------------------------------------------------------------------
myProbsList <- list()
myThetaRange <- seq(-4, 4, by = 0.1)
myModel <- "muraki"

for (i in seq_along(myThetaRange)) {
  myProbsList[[i]] <- simplep(myThetaRange[i], myItem, model = myModel)
}
myProbs <- (matrix(unlist(myProbsList), ncol = nrow(myItem), byrow = TRUE))
plot(myThetaRange, myProbs[, 2], type = "l")


## ----polyItem-----------------------------------------------------------------
library(conquestr)

myItem <- matrix(
  c(
    0, 0, 0    , 1.5, 
    1, 1, 0.2  , 1.5, 
    2, 1, -0.2 , 1.5
  ), ncol = 4, byrow=TRUE
)
colnames(myItem)<- c("k", "d", "t", "a")
print(myItem)

## ----polyProbs----------------------------------------------------------------
myProbs <- simplep(0.5, myItem)
print(myProbs)

## ----polyICC------------------------------------------------------------------
myProbsList <- list()
myModel <- "muraki"

for (i in seq_along(myThetaRange)) {
  myProbsList[[i]] <- simplep(myThetaRange[i], myItem, model = myModel)
}
myProbs <- (matrix(unlist(myProbsList), ncol = nrow(myItem), byrow = TRUE))
plot(myThetaRange, myProbs[,1], type = "l")
lines(myThetaRange, myProbs[,2])
lines(myThetaRange, myProbs[,3])
twoPLScaled_locations <- {
  if (myModel == "gpcm") {
    c(myItem[2, 2], sum(myItem[2, 2:3]), sum(myItem[3, 2:3]))
  } else {
    c(myItem[2, 2], sum(myItem[2, 2:3]), sum(myItem[3, 2:3]))/myItem[2, 4]
  }
}
abline(v = twoPLScaled_locations)

## ----Expected-----------------------------------------------------------------
library(conquestr)
myItems <- list()
myItems[[1]] <- matrix(c(
  0, 0, 0   , 1,
  1, 1, -0.2, 1,
  2, 1, 0.2 , 1
), ncol = 4, byrow = TRUE)
myItems[[2]] <- matrix(c(
  0, 0 , 0   , 1,
  1, -1, -0.4, 1,
  2, -1, 0.4 , 1
), ncol = 4, byrow = TRUE)
myItems[[3]] <- matrix(c(
  0, 0   , 0   , 1,
  1, 1.25, -0.6, 1,
  2, 1.25, 0.6 , 1
), ncol = 4, byrow = TRUE)
myItems[[4]] <- matrix(c(
  0, 0, 0   , 1, 
  1, 2, 0.2 , 1,
  2, 2, -0.2, 1
), ncol = 4, byrow = TRUE)
myItems[[5]] <- matrix(c(
  0, 0   , 0   , 1,
  1, -2.5, -0.2, 1,
  2, -2.5, 0.2 , 1
), ncol =  4, byrow = TRUE)
for (i in seq(myItems)) {
  colnames(myItems[[i]]) <- c("k", "d", "t", "a")
}
print(myItems)

expectedRes <- list()
for (i in seq_along(myThetaRange)) {
  tmpExp <- 0
  for (j in seq(myItems)) {
    tmpE <- simplef(myThetaRange[i], myItems[[j]])
    tmpExp <- tmpExp + tmpE
  }
  expectedRes[[i]] <- tmpExp
}

plot(myThetaRange, unlist(expectedRes), type = "l")


