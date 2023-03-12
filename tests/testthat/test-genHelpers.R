# check we can generate some item lists and appropriate errors are thrown
myDeltaDots <- data.frame(
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
    expect_equal(length(myGoodItemList_dich), length(myDeltaDots$id))
    expect_equal(length(myGoodItemList), length(myDeltaDots$id))
  }
)