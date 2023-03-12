## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plotRout-----------------------------------------------------------------
library(conquestr)
myRout <- ConQuestRout()
plotRout(myRout)

## ----infoWrightMap------------------------------------------------------------
myDeltaDots <- data.frame(
  id = c(1:10),
  itemid = paste0("item", 1:10),
  delta = rnorm(10)
)

MyTaus <- data.frame(
  id = c(2L, 10L),
  itemId = NA,
  step = c(1L, 1L),
  tau = rnorm(2)
)

myItemList <- makeItemList(deltaDot = myDeltaDots, tau = MyTaus)

myPersons <- rnorm(500, 1, 1)
informationWrightMap(myItems = myItemList, myAbilities = myPersons, minTheta = -6, maxTheta = 6)

