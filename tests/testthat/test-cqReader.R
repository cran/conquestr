int_le <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
dbl_le <- function(x) writeBin(as.double(x), raw(), size = 8, endian = "little")
str_cq <- function(s) c(int_le(nchar(s, "bytes")), charToRaw(s))

test_that("primitives are read little-endian and in order", {
  r <- .cqReaderNew(c(int_le(-7L), dbl_le(2.5), as.raw(1), str_cq("abc")))
  expect_identical(ReadInteger(r), -7L)
  expect_identical(ReadDouble(r), 2.5)
  expect_identical(ReadBoolean(r), TRUE)
  expect_identical(ReadString(r), "abc")
})

test_that("strings with embedded NULs become blanks", {
  r <- .cqReaderNew(c(int_le(3L), charToRaw("a"), as.raw(0), charToRaw("b")))
  expect_identical(ReadString(r), "   ")
  expect_identical(ReadString(.cqReaderNew(int_le(0L))), "")
})

test_that("reading past the end of the file errors", {
  expect_error(ReadInteger(.cqReaderNew(raw(2))), "unexpected end of file")
  expect_error(ReadString(.cqReaderNew(int_le(10L))), "unexpected end of file")
})

test_that("matrices are filled by row and keep dimnames", {
  bytes <- c(
    int_le(2L), int_le(2L), as.raw(0), str_cq("title"), # rows, cols, empty, title
    int_le(2L), str_cq("c1"), str_cq("c2"),             # column names
    int_le(2L), str_cq("r1"), str_cq("r2"),             # row names
    dbl_le(1:4)                                         # data, by row
  )
  m <- ReadMatrix(.cqReaderNew(bytes))
  expect_identical(unname(m), matrix(c(1, 2, 3, 4), 2, byrow = TRUE))
  expect_identical(dimnames(m), list(c("r1", "r2"), c("c1", "c2")))
})

test_that("readers reject objects that are not a cqReader", {
  con <- rawConnection(int_le(1L))
  on.exit(close(con))
  expect_error(ReadInteger(con), "cqReader")
})
