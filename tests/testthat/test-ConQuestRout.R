# call conquest to create rout object the default is an ICC
myRoutTest <- suppressMessages(conquestr::ConQuestRout())
myRoutDfTest <- conquestr::routPointsToDf(myRoutTest)
myRoutPlot <- conquestr::plotRout(myRoutTest)

# Rout tests
test_that("Rout file is of correct type", {
  expect_s3_class(myRoutTest, "ICC")
  expect_type(myRoutDfTest, "list")
  ggplot2::is_ggplot(myRoutPlot)
  }
)

