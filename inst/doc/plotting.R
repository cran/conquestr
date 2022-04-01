## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----plotRout-----------------------------------------------------------------
library(conquestr)
myRout<- ConQuestRout()
plotRout(myRout)   

## ----infoWrightMap------------------------------------------------------------
myItems<- runif(10, -1, 1)
myPersons<- rnorm(500, 1, 1)
informationWrightMap(myItems = myItems, myAbilities = myPersons, minTheta = -6, maxTheta = 6)

