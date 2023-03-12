# call conquest to create rout object the default is an ICC
mySys <- conquestr::ConQuestSys()
MyItemMap <- conquestr::plotItemMap(mySys)

# Rout tests
test_that("Item mape is of correct type", {
  expect_s3_class(MyItemMap, "ggplot")
  }
)