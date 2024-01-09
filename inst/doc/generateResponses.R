## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  error = FALSE
)

## ----slmItem, fig.width=7, fig.height=6---------------------------------------
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
myResponses <- genResponses(abilities = myAbils, itemParams = myItems)
print(head(cbind(myResponses[, 1:2], myAbils)))

## ----moreItems, fig.width=7, fig.height=6-------------------------------------
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

myResponses <- genResponses(
  abilities = myAbilsMat, itemParams = myItems, perturbR = NULL,
  groups = NULL, BMatrix = myBMatrix
)

print(cbind(myResponses[1:10, 1:5], myAbilsMat[1:10, 1:2]))

## ----misFit, fig.width=7, fig.height=6----------------------------------------
library(conquestr)
library(ggplot2)
library(dplyr)

myN <- 2000
myMean <- 0
mySd <- 2
myGroups <- c("gfit", "bfit")

# abilities
myAbilities <- rnorm(myN, myMean, mySd)
# groups
myData <- data.frame(
  ability = myAbilities,
  group = factor(sample(x = myGroups, size = myN, replace = TRUE))
)
# weights
myData$weight <- ifelse(myData$group == myGroups[1], 1, 0.001)
myData_copy <- myData # for use later

# items
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
myItems[[6]] <- matrix(c(
  0, 0  , 0 , 1,
  1, 1.5, 0 , 1
), ncol =  4, byrow = TRUE)
for (i in seq(myItems)) {
  colnames(myItems[[i]]) <- c("k", "d", "t", "a")
}


# Specify misfit.
myPertubations<- list()
myPertubations[[1]] <- list()
myPertubations[[1]] <- append(myPertubations[[1]], 1L)
myPertubations[[1]] <- append(myPertubations[[1]], "discrimination")
myPertubations[[1]] <- append(myPertubations[[1]],  0.50)
myPertubations[[1]] <- append(myPertubations[[1]], 0)
myPertubations[[1]] <- append(myPertubations[[1]], "bfit")
names(myPertubations[[1]]) <- c("item", "type", "factor", "pivot", "group")

myPertubations[[2]] <- list()
myPertubations[[2]] <- append(myPertubations[[2]], 2L)
myPertubations[[2]] <- append(myPertubations[[2]], "discrimination")
myPertubations[[2]] <- append(myPertubations[[2]],  1.75)
myPertubations[[2]] <- append(myPertubations[[2]], -1)
myPertubations[[2]] <- append(myPertubations[[2]], "bfit")
names(myPertubations[[2]]) <- c("item", "type", "factor", "pivot", "group")

myPertubations[[3]] <- list()
myPertubations[[3]] <- append(myPertubations[[3]], 3L)
myPertubations[[3]] <- append(myPertubations[[3]], "discrimination")
myPertubations[[3]] <- append(myPertubations[[3]],  1.75)
myPertubations[[3]] <- append(myPertubations[[3]], -5)
myPertubations[[3]] <- append(myPertubations[[3]], "bfit")
names(myPertubations[[3]]) <- c("item", "type", "factor", "pivot", "group")

myPertubations[[4]] <- list()
myPertubations[[4]] <- append(myPertubations[[4]], 6L)
myPertubations[[4]] <- append(myPertubations[[4]], "discrimination")
myPertubations[[4]] <- append(myPertubations[[4]], 2.5)
myPertubations[[4]] <- append(myPertubations[[4]], 0)
myPertubations[[4]] <- append(myPertubations[[4]], "bfit")
names(myPertubations[[4]]) <- c("item", "type", "factor", "pivot", "group")

myResponses <- genResponses(
  abilities = myData$ability, itemParams = myItems, perturbR = myPertubations,
  groups = NULL
)

myResponsesDf <- data.frame(myResponses)
names(myResponsesDf) <- paste0("item", 1:length(myItems))

myData1 <- bind_cols(myData, myResponsesDf)
myData <- myData1 

myCCC_item2 <- plotCCC(
  item = myItems[[2]], abilities = myData$ability, responses = myData$item2,
  weights = NULL, groups = NULL, bins = 10, range = c(-4, 7)
)

myCCC_item3 <- plotCCC(
  item = myItems[[3]], abilities = myData$ability, responses = myData$item3,
  weights = NULL, groups = NULL, bins = 10, range = c(-4, 7)
)


## ----fig.width=7, fig.height=6------------------------------------------------
library(gridExtra)
grid.arrange(myCCC_item2, myCCC_item3)

## ----misFit_groups, fig.width=7, fig.height=6---------------------------------
myResponses <- genResponses(
  abilities = myData_copy$ability, itemParams = myItems, perturbR = myPertubations,
  groups = myData_copy$group
)

myResponsesDf <- data.frame(myResponses)
names(myResponsesDf) <- paste0("item", 1:length(myItems))

myData2 <- bind_cols(myData_copy, myResponsesDf)
myData <- myData2 # overwrites previous examples


## ----misFit_groups_noWeights, fig.width=7, fig.height=6-----------------------
myCCC_item3a <- plotCCC(
  item = myItems[[3]], abilities = myData$ability, responses = myData$item3,
  weights = myData$weight, groups = myData$group, bins = 10, range = c(-4, 7)
)
plot(myCCC_item3a)

## ----misFit_noGroups_withWeights, fig.width=7, fig.height=6-------------------
myCCC_item3b <- plotCCC(
  item = myItems[[3]], abilities = myData$ability, responses = myData$item3,
  weights = myData$weight, groups = NULL, bins = 10, range = c(-4, 7)
)
plot(myCCC_item3b)

## ----misFit_noGroups_noWeights, fig.width=7, fig.height=6---------------------
myCCC_item3c <- plotCCC(
  item = myItems[[3]], abilities = myData$ability, responses = myData$item3,
  weights = NULL, groups = NULL, bins = 10, range = c(-4, 7)
)
plot(myCCC_item3c)

