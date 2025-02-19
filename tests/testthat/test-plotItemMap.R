# call conquestr to create default system file 
mySys <- suppressMessages(conquestr::ConQuestSys())
MyItemMap <- conquestr::plotItemMap(mySys)

# Rout tests
test_that("Item mape is of correct type", {
  expect_s3_class(MyItemMap, "ggplot")
  }
)