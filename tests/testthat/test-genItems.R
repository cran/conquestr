# check we can generate some item lists and appropriate errors are thrown
myNItems <- 50L
myDeltadots <- list()
myDeltadots[[1]] <- "runif"
myDeltadots[[2]] <- list(min = -2.5, max = 5)
myItems <- genItems(myNItems, centre = 0, deltadots = myDeltadots) 

myDeltadots2 <- list()
myDeltadots2[[1]] <- "rnorm"
myDeltadots2[[2]] <- list(mu = -1, sd = 2)
myItems2 <- genItems(myNItems, centre = NULL, deltadots = myDeltadots2) 

myTaus <- list()
myTaus[[1]] <- "rnorm"
myTaus[[2]] <- list(mu = -1, sd = 2)
myTaus[[3]] <- TRUE
myItems3 <- genItems(myNItems, centre = NULL, deltadots = myDeltadots2, taus = myTaus) 

myScores <- list()
myScores[[1]] <- "rnorm"
myScores[[2]] <- list(mu = -1, sd = 2)
myScores[[3]] <- TRUE
myScores[[4]] <- TRUE
myScores[[5]] <- c(1:(floor(myNItems/2)), myNItems)

myItems4 <- genItems(myNItems, centre = 0, deltadots = myDeltadots, taus = myTaus, discrims = myScores) 


# check centering
myItemsDf <- makeItemDfs(myItems)
myItemsDf2 <- makeItemDfs(myItems2)
myItemsDf3 <- makeItemDfs(myItems3)
myItemsDf4 <- makeItemDfs(myItems4)

test_that(
  "dichotomous items are generated", {
    expect_true(
      is.matrix(myItems[[length(myItems)]])
    )
    expect_type(
      myItems, 
      "list"
    )
    expect_type(
      myItems3, 
      "list"
    )
    expect_equal(length(myItems), myNItems)
    expect_equal(length(myItems2), myNItems)
    expect_equal(length(myItems3), myNItems)
    expect_equal(length(myItems4), myNItems)
  }
)

test_that(
  "check centring", {
    expect_equal(
      mean(myItemsDf[[2]]$delta),
      0
    )
    expect_failure(
      expect_equal(
        mean(myItemsDf2[[2]]$delta),
        0
      )
    )
  }
)