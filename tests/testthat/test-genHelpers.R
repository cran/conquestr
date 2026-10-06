# check we can generate some item lists and appropriate errors are thrown
myDeltaDots <- data.frame(
  id = c(1L:10),
  itemid = paste0("item", 1:10),
  delta = rnorm(10)
)

# numeric "id" should be cast to Integer internally
myDeltaDotsBad <- data.frame(
  id = c(1L:10),
  itemid = paste0("item", 1:10),
  delta = rnorm(10)
)


MyBadTaus <- data.frame(
  # there is no item 22 in myDeltaDots
  id = c(2L, 22L),
  itemid = NA,
  step = c(1, 1),
  tau = rnorm(2)
)

MyGoodTaus <- data.frame(
  id = c(2L, 10L),
  itemid = NA,
  step = c(1L, 1L),
  tau = rnorm(2)
)

# makeItemList tests
test_that(
  "Errors are thrown", {
    expect_error(
      makeItemList(deltaDot = myDeltaDots, tau = MyBadTaus)
    )
    expect_error(
      makeItemList(deltaDot = myDeltaDots, discrim = "1")
    )
    expect_type(
      myGoodItemList_dich <- makeItemList(deltaDot = myDeltaDots),
      "list"
    )
    expect_type(
      myGoodItemList <- makeItemList(deltaDot = myDeltaDots, tau = MyGoodTaus),
      "list"
    )
    expect_type(
      myItemListCastIdToInt <- makeItemList(deltaDot = myDeltaDotsBad, tau = MyGoodTaus),
      "list"
    )
    expect_equal(length(myGoodItemList_dich), length(myDeltaDots$id))
    expect_equal(length(myGoodItemList), length(myDeltaDots$id))
  }
)

# simplep, simplef, cnvrtItemParam consistency tests
#
# a partial credit item estimated in ConQuest's parameterisation (D = 1)
myItemConquest <- matrix(
  c(
    0, -0.87792,  0.000, 1.78768,
    1, -0.87792, -2.654, 1.78768,
    2, -0.87792, -0.367, 1.78768,
    3, -0.87792,  3.021, 1.78768
  ), ncol = 4, byrow = TRUE
)
colnames(myItemConquest) <- c("x", "b", "d", "a")

myItemMuraki <- cnvrtItemParam(
  myItemConquest, from = "conquest", to = "muraki", DFrom = 1, DTo = 1.7
)

test_that(
  "cnvrtItemParam round-trips item parameters", {
    myItemConquestRoundTrip <- cnvrtItemParam(
      myItemMuraki, from = "muraki", to = "conquest", DFrom = 1.7, DTo = 1
    )
    expect_equal(myItemConquestRoundTrip, myItemConquest)
  }
)

test_that(
  "simplep gives the same probabilities under conquest (D = 1) and muraki (D = 1.7) parameterisations", {
    for (myTheta in c(-2, -0.5, 0, 0.5, 1.5)) {
      probsConquest <- simplep(theta = myTheta, item = myItemConquest, model = "conquest", D = 1)
      probsMuraki <- simplep(theta = myTheta, item = myItemMuraki, model = "muraki", D = 1.7)
      expect_equal(as.vector(probsConquest), as.vector(probsMuraki), tolerance = 1e-8)
      # probabilities for a single item should always sum to 1
      expect_equal(sum(probsConquest), 1, tolerance = 1e-8)
      expect_equal(sum(probsMuraki), 1, tolerance = 1e-8)
    }
  }
)

test_that(
  "simplef gives the same expected score under conquest and muraki parameterisations", {
    for (myTheta in c(-2, -0.5, 0, 0.5, 1.5)) {
      expectConquest <- simplef(theta = myTheta, item = myItemConquest, model = "conquest", D = 1)
      expectMuraki <- simplef(theta = myTheta, item = myItemMuraki, model = "muraki", D = 1.7)
      expect_equal(expectConquest, expectMuraki, tolerance = 1e-8)
      # expected score must lie within the range of the category scores
      expect_true(expectConquest >= min(myItemConquest[, 1]) && expectConquest <= max(myItemConquest[, 1]))
    }
  }
)

# genResponses perturbR tests
#
# Responses are stochastic, so the size of the misfit in any one sample is not
# deterministic. Instead, observed mean scores are compared to model-implied
# expected scores within a tolerance based on their sampling error (4 SE).
test_that(
  "genResponses perturbR induces misfit in one item, for the perturbed group only", {
    set.seed(20240411)
    myN <- 10000
    myData <- data.frame(
      ability = rnorm(myN, 0, 2),
      group = factor(sample(c("gfit", "bfit"), myN, replace = TRUE))
    )

    # two identical dichotomies, only item 2 is perturbed
    myItem <- matrix(c(
      0, 0  , 0, 1,
      1, 0.5, 0, 1
    ), ncol = 4, byrow = TRUE)
    colnames(myItem) <- c("k", "d", "t", "a")
    myItems <- list(myItem, myItem)

    # for group bfit, shift the location of item 2 one logit to the right
    myPerturbations <- list(
      list(item = 2L, type = "shift", factor = 1, pivot = 1, group = "bfit")
    )
    myItemShifted <- myItem
    myItemShifted[2, 2] <- myItemShifted[2, 2] + 1

    myResponses <- genResponses(
      abilities = myData$ability, itemParams = myItems,
      perturbR = myPerturbations, groups = myData$group
    )
    expect_equal(dim(myResponses), c(myN, 2))
    expect_true(all(myResponses %in% c(0, 1)))

    # observed mean score, expected mean score, and SE of the observed mean
    obsVsExp <- function(item, resp, theta) {
      p <- vapply(theta, simplef, numeric(1), item = item)
      list(
        obs = mean(resp),
        exp = mean(p),
        se = sqrt(sum(p * (1 - p))) / length(p)
      )
    }

    isG <- myData$group == "gfit"
    isB <- myData$group == "bfit"

    # unperturbed item fits in both groups
    for (thisGroup in list(isG, isB)) {
      r <- obsVsExp(myItem, myResponses[thisGroup, 1], myData$ability[thisGroup])
      expect_lt(abs(r$obs - r$exp), 4 * r$se)
    }

    # perturbed item fits the original model in gfit
    r <- obsVsExp(myItem, myResponses[isG, 2], myData$ability[isG])
    expect_lt(abs(r$obs - r$exp), 4 * r$se)

    # perturbed item misfits the original model in bfit (harder than expected) ...
    r <- obsVsExp(myItem, myResponses[isB, 2], myData$ability[isB])
    expect_lt(r$obs, r$exp - 4 * r$se)

    # ... and fits the shifted item in bfit
    r <- obsVsExp(myItemShifted, myResponses[isB, 2], myData$ability[isB])
    expect_lt(abs(r$obs - r$exp), 4 * r$se)
  }
)