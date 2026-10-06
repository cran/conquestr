cmpSysPath <- system.file("extdata", "mysysfile.cqs", package = "conquestr")
cmpSysBytes <- readBin(cmpSysPath, what = "raw", n = file.size(cmpSysPath))
uncmpHeader <- c(as.raw(c(0x0C, 0x00, 0x00, 0x00)), charToRaw("uncompressed"))

test_that("example system file is a zlib stream", {
  expect_identical(cmpSysBytes[1:2], as.raw(c(0x78, 0xda)))
})

test_that("DecompressSys prepends the 'uncompressed' header", {
  out <- DecompressSys(cmpSysBytes)
  expect_type(out, "raw")
  expect_identical(out[seq_along(uncmpHeader)], uncmpHeader)
  expect_identical(ReadString(.cqReaderNew(out)), "uncompressed")
})

test_that("DecompressSys matches an independent zlib implementation", {
  out <- DecompressSys(cmpSysBytes)
  expect_identical(out[-seq_along(uncmpHeader)], memDecompress(cmpSysBytes, type = "gzip"))
  expect_gt(length(out), length(cmpSysBytes))
})

test_that("DecompressSys round-trips arbitrary payloads", {
  set.seed(1)
  payloads <- list(
    empty = raw(0),
    text = charToRaw("ACER ConQuest"),
    # larger than any internal buffer, incompressible
    random = as.raw(sample(0:255, 5e5, replace = TRUE)),
    # highly compressible
    zeros = raw(1e6)
  )
  for (nm in names(payloads)) {
    p <- payloads[[nm]]
    expect_identical(
      DecompressSys(memCompress(p, type = "gzip")),
      c(uncmpHeader, p),
      label = nm
    )
  }
})

test_that("DecompressSys errors on input that is not a valid zlib stream", {
  # zlib writes its own diagnostics to stderr; keep test output clean
  quietly <- function(x) utils::capture.output(DecompressSys(x), type = "message")
  expect_error(quietly(raw(0)), "Decompression failed")
  expect_error(quietly(as.raw(c(0x78, 0xda, 1:20))), "Decompression failed")
  expect_error(quietly(charToRaw("not a system file")), "Decompression failed")
})

test_that("ConQuestSys gives the same result for compressed and uncompressed files", {
  tmp <- tempfile(fileext = ".cqs")
  on.exit(unlink(tmp))
  writeBin(DecompressSys(cmpSysBytes), tmp)

  msgs <- capture_messages(fromCmp <- ConQuestSys(cmpSysPath))
  expect_match(msgs, "System file is compressed", all = FALSE)
  expect_no_message(fromUncmp <- ConQuestSys(tmp))
  expect_identical(fromCmp, fromUncmp)
  expect_identical(fromCmp$compressedString, "uncompressed")
})

test_that("ConQuestSys errors on a truncated compressed file", {
  tmp <- tempfile(fileext = ".cqs")
  on.exit(unlink(tmp))
  writeBin(cmpSysBytes[seq_len(length(cmpSysBytes) %/% 2)], tmp)

  expect_error(suppressMessages(ConQuestSys(tmp)))
})
