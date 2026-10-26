test_that("Resp Model is correct for an unidentified model", {
  MySys_unident <- ConQuestSys(
    system.file("extdata", "mysysfile_unident.cqs", package = "conquestr")
  )
  MyRespunident <- getCqRespModel(MySys_unident)
  expect_s3_class(MyRespunident, "data.frame")
  # gin 0 is not identified, so only gins 1-12 have parameters
  expect_setequal(MyRespunident$gin_no, c(1:12))
  expect_false(0 %in% MyRespunident$gin_no)
  # one row per fit statistic, and long labels line up with gins (0-offset)
  expect_equal(nrow(MyRespunident), length(MySys_unident$gFitStatistics$Value))
  expect_equal(
    MyRespunident$GinLongLabel,
    unlist(MySys_unident$gGinLongLabels)[MyRespunident$gin_no + 1]
  )
  # the first gin is anchored, so has no SE
  expect_true(is.na(MyRespunident$se[1]))
  }
)
