# call conquestr to create itan from default system file
MySys_unident <- ConQuestSys(system.file("extdata", "mysysfile_unident.cqs", package = "conquestr"))

# commented out pending fixes to 1.6
# MyRespunident <- getCqRespModel(MySys_unident)

# # Rout tests
# test_that("Resp Model is correct", {
#   expect_s3_class(MyRespunident, "data.frame")
#   expect_setequal(MyRespunident$gin_no, c(2:12))
#   expect_true(is.na(MyRespunident$se[1]))
#   }
# )

