# call conquest to create rout object the default is an ICC
mySys <- system.file("extdata", "mysysfile.cqs", package = "conquestr")
mySysOld <- system.file("extdata", "mysysfile_v26.cqs", package = "conquestr")
mySysTest <- suppressMessages(conquestr::ConQuestSys(mySys))
mySysTest_old <- suppressMessages(conquestr::ConQuestSys(mySysOld))


# Rout tests
test_that("System file is read", {
  expect_s3_class(mySysTest$gTokenList, "data.frame")
  expect_null(mySysTest_old$gTokenList, "data.frame")
  }
)

