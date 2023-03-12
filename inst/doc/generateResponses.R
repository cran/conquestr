## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----slmItem------------------------------------------------------------------
library(conquestr)
myItem <- matrix(c(0, 0, 0, 0, 1, 1, 0, 1), ncol = 4, byrow = TRUE)
colnames(myItem) <- c("k", "d", "t", "a")
print(myItem)
plotModelCCC(myItem)

## ----slmItemList--------------------------------------------------------------
myItems <- list(myItem, myItem)
myItems[[2]][2, 2] <- -1 # make the second item delta equal to -1
print(plotModelExp(myItems))

## ----slmAbils-----------------------------------------------------------------
myAbils <- rnorm(100)

## ----slmResps-----------------------------------------------------------------
myResponses <- genResponses(myAbils, myItems)
print(cbind(myResponses[1:10, 1:2], myAbils[1:10]))

## ----moreItems----------------------------------------------------------------
myItemPoly <- matrix(
  c(
    0, 0.0,  0.0, 0,
    1, 0.5, -1.5, 1,
    2, 0.5,  1.5, 1
  ),
  ncol = 4, byrow = TRUE
)

myItem2Pl <- matrix(
  c(
    0,  0,  0.00, 0.00,
    1, -2, -1.25, 0.85,
    2, -2, -0.25, 0.85,
    3, -2,  0.30, 0.85,
    4, -2,  1.20, 0.85
  ),
  ncol = 4, byrow = TRUE
)

myItemNonIntScore <- matrix(
  c(
    0,   0,  0.00, 0,
    1.2, 0, -0.35, 1,
    1.8, 0,  0.15, 1,
    3.8, 0,  0.20, 1
  ),
  ncol = 4, byrow = TRUE
)

myItems <- append(myItems, list(myItemPoly, myItem2Pl, myItemNonIntScore))
plotModelCCC(myItemPoly)
plotModelExp(myItems)

## ----multiDimData-------------------------------------------------------------
myBMatrix <- matrix(
  c(
    1, 0,
    1, 0,
    0, 1,
    0, 1,
    0, 1
  ),
  byrow = TRUE,
  ncol = 2
)

myAbils2 <- rnorm(length(myAbils))
myAbilsMat <- cbind(myAbils, myAbils2) # assumes expected correlation = 0

myResponses <- genResponses(myAbilsMat, myItems, BMatrix = myBMatrix)
print(cbind(myResponses[1:10, 1:5], myAbilsMat[1:10, 1:2]))

