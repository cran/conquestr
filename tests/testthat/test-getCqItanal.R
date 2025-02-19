# call conquestr to create itan from default system file
mySysTest <- suppressMessages(conquestr::ConQuestSys())
myItn <- conquestr::getCqItanal(mySysTest)
myTestItnTable <- myItn[[1]][[10]]$table
myTestItnSum <- myItn[[1]][[13]]

# Rout tests
test_that("Rout file is of correct type", {
  expect_s3_class(myItn, "cqItanal")
  expect_s3_class(myTestItnTable, "data.frame")
  expect_setequal(myTestItnTable$Count, c(776, 70, 57, 88, 8))
  expect_equal(myTestItnSum$Alpha, 0.64909436, tolerance = 0.01)
  }
)

