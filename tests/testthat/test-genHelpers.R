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
# genResponses debug output and group-specific perturbations
#
# Shared fixture: three groups, a dichotomous and a partial credit item
genDebugFixture <- function() {
  set.seed(6203)
  myN <- 300
  myItem1 <- matrix(c(0, 0, 0, 1, 1, 0.5, 0, 1), ncol = 4, byrow = TRUE)
  myItem2 <- matrix(c(
    0, 0   ,  0  , 1,
    1, -0.2, -0.4, 1,
    2, -0.2,  0.4, 1
  ), ncol = 4, byrow = TRUE)
  list(
    abilities = rnorm(myN),
    groups = factor(sample(c("g1", "g2", "g3"), myN, replace = TRUE)),
    items = list(myItem1, myItem2)
  )
}

test_that("debug = TRUE returns the same responses as debug = FALSE", {
  fx <- genDebugFixture()
  myPert <- list(list(item = 2L, type = "discrimination", factor = 1.5, pivot = 0.5, group = "g2"))
  set.seed(1971)
  plain <- genResponses(
    abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
    perturbR = myPert, mcarP = 0.1
  )
  set.seed(1971)
  dbg <- genResponses(
    abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
    perturbR = myPert, mcarP = 0.1, debug = TRUE
  )
  expect_identical(dbg$responses, plain)
  expect_identical(is.na(dbg$responses), dbg$missing)
  expect_identical(dbg$responses[!dbg$missing], dbg$responsesComplete[!dbg$missing])
})

test_that("perturbation applies to all cases, one group, or several groups", {
  fx <- genDebugFixture()
  mapFor <- function(group, groups = fx$groups) {
    p <- list(item = 1L, type = "shift", pivot = 1)
    if (!missing(group)) p$group <- group
    genResponses(
      abilities = fx$abilities, groups = groups, itemParams = fx$items,
      perturbR = list(p), debug = TRUE
    )$perturbMap
  }
  # no group given, or groups not supplied: all cases
  expect_true(all(mapFor()[, 1] == 1L))
  expect_true(all(mapFor(NA)[, 1] == 1L))
  expect_true(all(mapFor("g1", groups = NULL)[, 1] == 1L))
  # one group
  expect_identical(mapFor("g2")[, 1] == 1L, fx$groups == "g2")
  # several groups
  expect_identical(mapFor(c("g1", "g3"))[, 1] == 1L, fx$groups %in% c("g1", "g3"))
  # unperturbed items are never perturbed
  expect_true(all(mapFor(c("g1", "g3"))[, 2] == 0L))
  # unnamed elements: groups from the fifth element onwards
  dbg <- genResponses(
    abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
    perturbR = list(list(1L, "shift", 1, 1, "g1", "g3")), debug = TRUE
  )
  expect_identical(dbg$perturbMap[, 1] == 1L, fx$groups %in% c("g1", "g3"))
})

test_that("response probabilities are perturbed for the intended cases only", {
  fx <- genDebugFixture()
  myPert <- list(
    list(item = 1L, type = "shift", pivot = 0.8, group = c("g1", "g2")),
    list(item = 2L, type = "discrimination", factor = 1.6, pivot = -0.3, group = "g3")
  )
  dbg <- genResponses(
    abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
    perturbR = myPert, debug = TRUE
  )
  probs <- genResponsesProbs(dbg)
  expect_equal(nrow(probs), length(fx$abilities) * (2 + 3))

  # recompute every probability independently with simplep()
  expectedItem <- function(i) {
    ifelse(
      probs$perturbation[i] == 0,
      list(fx$items[[probs$item[i]]]),
      list(dbg$perturbedItems[[probs$perturbation[i]]])
    )[[1]]
  }
  recomputed <- vapply(seq_len(nrow(probs)), function(i) {
    simplep(probs$theta[i], expectedItem(i))[probs$category[i]]
  }, numeric(1))
  expect_equal(probs$prob, recomputed, tolerance = 1e-12)
  recomputedBase <- vapply(seq_len(nrow(probs)), function(i) {
    simplep(probs$theta[i], fx$items[[probs$item[i]]])[probs$category[i]]
  }, numeric(1))
  expect_equal(probs$probBase, recomputedBase, tolerance = 1e-12)

  # perturbed rows differ from the base, unperturbed rows do not
  isPert <- probs$perturbation > 0
  expect_identical(
    isPert,
    (probs$item == 1 & probs$group %in% c("g1", "g2")) |
      (probs$item == 2 & probs$group == "g3")
  )
  expect_true(all(abs(probs$prob[isPert] - probs$probBase[isPert]) > 0))
  expect_identical(probs$prob[!isPert], probs$probBase[!isPert])

  # the drawn response is the category whose cumulative probability first exceeds the draw
  for (j in 1:2) {
    pj <- probs[probs$item == j, ]
    cdf <- ave(pj$prob, pj$case, FUN = cumsum)
    firstAbove <- tapply(seq_len(nrow(pj)), pj$case, function(r) {
      pj$score[r][which(pj$draw[r[1]] < cdf[r])[1]]
    })
    expect_equal(as.vector(firstAbove), dbg$responsesComplete[, j])
  }
})

test_that("perturbItem applies shift and discrimination as documented", {
  myItem <- matrix(c(0, 0, 0, 1, 1, 0.5, 0, 1), ncol = 4, byrow = TRUE)
  shifted <- perturbItem(myItem, type = "shift", factor = 99, pivot = 1)
  expect_equal(shifted[, 2], c(0, 1.5))
  expect_equal(shifted[, 4], myItem[, 4]) # factor ignored for shift
  disc <- perturbItem(myItem, type = "discrimination", factor = 2, pivot = -0.5)
  expect_equal(disc[, 4], c(2, 2))
  expect_equal(disc[, 2], c(0, 0))
  # dichotomy: perturbed and base curves cross at theta = pivot / (a * (factor - 1))
  myItem2 <- myItem
  myItem2[, 4] <- 1.5
  disc2 <- perturbItem(myItem2, type = "discrimination", factor = 2, pivot = 0.6)
  crossTheta <- 0.6 / (1.5 * (2 - 1))
  expect_equal(simplep(crossTheta, disc2), simplep(crossTheta, myItem2), tolerance = 1e-12)
})

test_that("perturbation inputs are validated", {
  fx <- genDebugFixture()
  gen <- function(p, groups = fx$groups) {
    genResponses(abilities = fx$abilities, groups = groups, itemParams = fx$items, perturbR = list(p))
  }
  expect_error(gen(list(item = 3L, type = "shift", pivot = 1)), "between 1 and 2")
  expect_error(gen(list(item = 1L, type = "slope", pivot = 1)), "'discrimination' or 'shift'")
  expect_error(gen(list(item = 1L, type = "discrimination", pivot = 1)), "`factor`")
  expect_error(gen(list(item = 1L, type = "shift")), "`pivot`")
  expect_error(gen(list(item = 1L, type = "shift", pivot = 1, group = "g9")), "'g9'")
  expect_error(gen(list(item = 1L, type = "shift", pivot = 1, group = c("g1", NA))), "mix NA")
  expect_error(
    genResponses(abilities = fx$abilities, itemParams = fx$items, debug = "yes"),
    "`debug`"
  )
})

test_that("overlapping perturbations warn and the later one is used", {
  fx <- genDebugFixture()
  myPert <- list(
    list(item = 1L, type = "shift", pivot = 1),
    list(item = 1L, type = "shift", pivot = -1, group = "g2")
  )
  expect_warning(
    dbg <- genResponses(
      abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
      perturbR = myPert, debug = TRUE
    ),
    "perturbation 2 is used"
  )
  expect_identical(dbg$perturbMap[, 1], ifelse(fx$groups == "g2", 2L, 1L))
})

test_that("discrimination with pivotAt = 'item' keeps the item location fixed", {
  myDich <- matrix(c(0, 0, 0, 1.3, 1, 0.65, 0, 1.3), ncol = 4, byrow = TRUE) # location 0.5
  myPcm <- matrix(c(
    0, 0  ,  0  , 1.2,
    1, 0.6, -0.4, 1.2,
    2, 0.6,  0.4, 1.2
  ), ncol = 4, byrow = TRUE) # location 0.5
  for (myItem in list(myDich, myPcm)) {
    location <- mean(myItem[-1, 2] / myItem[-1, 4])
    for (myPivot in c(0, -1, 0.75)) {
      pert <- perturbItem(
        myItem, type = "discrimination", factor = 1.8, pivot = myPivot, pivotAt = "item"
      )
      expect_equal(pert[, 4], myItem[, 4] * 1.8)
      # all category probabilities are unchanged at the pivot point
      expect_equal(
        simplep(location + myPivot, pert), simplep(location + myPivot, myItem),
        tolerance = 1e-12
      )
      # and differ elsewhere
      expect_false(isTRUE(all.equal(simplep(location + myPivot + 1, pert), simplep(location + myPivot + 1, myItem))))
    }
    # pivot = 0: the item location does not move
    pert0 <- perturbItem(myItem, type = "discrimination", factor = 0.5, pivot = 0, pivotAt = "item")
    expect_equal(mean(pert0[-1, 2] / pert0[-1, 4]), location)
  }
})

test_that("discrimination shift moves the location without moving the pivot", {
  myItem <- matrix(c(0, 0, 0, 1, 1, 0.5, 0, 1), ncol = 4, byrow = TRUE)
  noShift <- perturbItem(myItem, "discrimination", factor = 2, pivot = 0, pivotAt = "item")
  withShift <- perturbItem(myItem, "discrimination", factor = 2, pivot = 0, pivotAt = "item", shift = 0.4)
  expect_equal(withShift[, 2] - noShift[, 2], c(0, 0.4))
  expect_equal(withShift[, 4], noShift[, 4])
})

test_that("pivotAt and shift are parsed and validated", {
  fx <- genDebugFixture()
  dbg <- genResponses(
    abilities = fx$abilities, groups = fx$groups, itemParams = fx$items,
    perturbR = list(
      list(item = 1L, type = "discrimination", factor = 2, pivot = 0, pivotAt = "item", shift = 0.3),
      list(item = 2L, type = "discrimination", factor = 2, pivot = 0.5),
      list(item = 2L, type = "shift", pivot = 1, group = "g2"),
      list(1L, "discrimination", 2, 0.5, "g3")
    ),
    debug = TRUE
  ) |> suppressWarnings()
  expect_equal(dbg$perturbations$pivotAt, c("item", "theta", NA, "theta"))
  expect_equal(dbg$perturbations$shift, c(0.3, 0, 0, 0))
  expect_equal(
    dbg$perturbedItems[[1]],
    perturbItem(fx$items[[1]], "discrimination", 2, 0, pivotAt = "item", shift = 0.3)
  )
  # default pivotAt = "theta" reproduces the previous behaviour
  expect_equal(dbg$perturbedItems[[2]][, 2], fx$items[[2]][, 2] + c(0, 0.5, 0.5))

  gen <- function(p) {
    genResponses(abilities = fx$abilities, itemParams = fx$items, perturbR = list(p))
  }
  expect_error(gen(list(item = 1L, type = "discrimination", factor = 2, pivot = 0, pivotAt = "x")), "`pivotAt`")
  expect_error(gen(list(item = 1L, type = "discrimination", factor = 2, pivot = 0, shift = "a")), "`shift`")
  expect_error(gen(list(item = 1L, type = "shift", pivot = 1, pivotAt = "item")), "only used by type")
})
