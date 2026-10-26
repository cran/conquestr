make_cor <- function(L = 5, n = 200, seed = 3907) {
  set.seed(seed)
  cor(matrix(rnorm(n * L), n, L))
}

# Rasch-like standardised residuals under local independence
make_resid <- function(n = 500, L = 10, seed = 8264) {
  set.seed(seed)
  theta <- rnorm(n)
  b <- seq(-1.5, 1.5, length.out = L)
  p <- plogis(outer(theta, b, "-"))
  x <- matrix(rbinom(n * L, 1, p), n, L)
  # Q3's -1/(L-1) bias arises from using estimated abilities: ML theta for the
  # Rasch model given known b (extreme scores dropped)
  x <- x[rowSums(x) > 0 & rowSums(x) < L, , drop = FALSE]
  th <- numeric(nrow(x))
  for (i in 1:30) {
    ph <- plogis(outer(th, b, "-"))
    th <- th + (rowSums(x) - rowSums(ph)) / rowSums(ph * (1 - ph))
  }
  ph <- plogis(outer(th, b, "-"))
  (x - ph) / sqrt(ph * (1 - ph))
}

# q3ExpCorrect ---------------------------------------------------------------

test_that("q3ExpCorrect shifts off-diagonals by +1/(L-1) and keeps the diagonal", {
  for (L in c(2, 3, 5, 20)) {
    r <- make_cor(L)
    out <- q3ExpCorrect(r)
    off <- row(r) != col(r)
    expect_equal(out[off], pmin(r[off] + 1 / (L - 1), 1))
    expect_equal(diag(out), rep(1, L))
  }
})

test_that("q3ExpCorrect preserves symmetry and dimnames", {
  r <- make_cor(6)
  dimnames(r) <- list(letters[1:6], letters[1:6])
  out <- q3ExpCorrect(r)
  expect_true(isSymmetric(out))
  expect_identical(dimnames(out), dimnames(r))
})

test_that("q3ExpCorrect centres null residual correlations near zero", {
  # Yen's raw Q3 has expectation approx -1/(L-1); the adjusted values should not
  r <- cor(make_resid(n = 2000, L = 6))
  raw_mean <- mean(r[lower.tri(r)])
  adj <- q3ExpCorrect(r)
  expect_lt(raw_mean, 0)
  expect_lt(abs(mean(adj[lower.tri(adj)])), abs(raw_mean))
})

test_that("q3ExpCorrect clamps to [-1, 1]", {
  r <- matrix(0.9, 3, 3); diag(r) <- 1
  out <- q3ExpCorrect(r)
  expect_true(all(out <= 1 & out >= -1))
  expect_equal(out[1, 2], 1)
})

test_that("q3ExpCorrect rejects non-correlation matrices", {
  expect_error(q3ExpCorrect(matrix(2, 3, 3)), "correlation matrix")
})

test_that("q3ExpCorrect tolerates floating-point diagonals", {
  r <- make_cor(4)
  diag(r) <- 1 + 1e-15
  expect_no_error(q3ExpCorrect(r))
})

test_that("q3ExpCorrect gives an informative error for NA (zero-variance item)", {
  r <- make_cor(4)
  r[2, ] <- r[, 2] <- NA
  expect_error(q3ExpCorrect(r), "correlation matrix")
})

test_that("q3ExpCorrect is undefined for a 1x1 matrix", {
  # correction = -1/0; should error rather than return silently
  expect_error(q3ExpCorrect(matrix(1)))
})

# fisherTrnsfrm --------------------------------------------------------------

test_that("fisherTrnsfrm applies atanh off-diagonal and leaves diagonal", {
  r <- make_cor(5)
  z <- fisherTrnsfrm(r)
  off <- row(r) != col(r)
  expect_equal(z[off], atanh(r[off]))
  expect_equal(diag(z), rep(1, 5))
  expect_true(isSymmetric(z))
})

test_that("fisherTrnsfrm round-trips with tanh", {
  r <- make_cor(5)
  z <- fisherTrnsfrm(r)
  off <- row(r) != col(r)
  expect_equal(tanh(z[off]), r[off])
})

test_that("fisherTrnsfrm does not leave off-diagonal +/-1 on the r scale", {
  # Off-diagonal values of +/-1 are skipped, so they stay as 1 (r scale)
  # while neighbours are on the z scale: atanh(0.99) = 2.65 > 1.
  r <- matrix(c(1, 1, 0.99,
                1, 1, 0,
                0.99, 0, 1), 3)
  expect_warning(z <- fisherTrnsfrm(r), "truncated")
  expect_gt(abs(z[1, 2]), abs(z[1, 3]))
  expect_true(all(is.finite(z)))
})

# steigerStat ----------------------------------------------------------------

test_that("steigerStat matches a manual calculation", {
  x <- make_resid()
  n <- nrow(x); L <- ncol(x)
  r <- cor(x) + 1 / (L - 1)
  diag(r) <- 1
  z <- atanh(r[lower.tri(r)])
  res <- steigerStat(x)
  expect_equal(res$steigerStat, (n - 3) * sum(z^2))
  expect_equal(res$DF, L * (L - 1) / 2)
  expect_equal(res$Chisq_p, pchisq(res$steigerStat, res$DF, lower.tail = FALSE))
})

test_that("steigerStat flags flags toggle the transforms", {
  x <- make_resid()
  r <- cor(x)
  n <- nrow(x)
  plain <- steigerStat(x, q3Adj = FALSE, fisher = FALSE)
  expect_equal(plain$Cor, r)
  expect_equal(plain$steigerStat, (n - 3) * sum(r[lower.tri(r)]^2))
})

test_that("steigerStat accepts data frames and matches matrix input", {
  x <- make_resid()
  expect_equal(steigerStat(as.data.frame(x))$steigerStat,
               steigerStat(x)$steigerStat)
})

test_that("steigerStat is roughly calibrated under local independence", {
  # Rejection rate at 5% should not be wildly off with the Q3 correction
  p <- vapply(1:200, function(s) {
    steigerStat(make_resid(n = 500, L = 8, seed = s))$Chisq_p
  }, numeric(1))
  expect_lt(mean(p < 0.05), 0.15)
})

test_that("steigerStat detects local dependence", {
  x <- make_resid()
  x[, 2] <- x[, 1] + rnorm(nrow(x), sd = 0.5)
  expect_lt(steigerStat(x)$Chisq_p, 0.001)
})

test_that("steigerStat stays finite when a corrected correlation hits 1", {
  x <- make_resid()
  x[, 2] <- x[, 1]
  expect_warning(res <- steigerStat(x), "truncated")
  expect_true(is.finite(res$steigerStat))
  # Pair (1,2) must contribute a large z, not 1
  expect_gt(abs(res$Cor[2, 1]), 5)
})

test_that("dfAdj warns only outside the calibrated N, L and tpm range", {
  x <- make_resid(n = 500, L = 10)
  expect_no_warning(steigerStat(x, dfAdj = TRUE, tpm = 0))
  expect_no_warning(steigerStat(x, dfAdj = TRUE, tpm = -1))
  expect_no_warning(steigerStat(x, dfAdj = FALSE, tpm = 3))
  expect_warning(steigerStat(x, dfAdj = TRUE, tpm = 0.5), "tpm = 0.5")
  expect_warning(steigerStat(x, dfAdj = TRUE, tpm = -2), "tpm = -2")
  expect_warning(steigerStat(make_resid(n = 500, L = 5), dfAdj = TRUE), "L = 5")
  expect_warning(steigerStat(make_resid(n = 3000, L = 10), dfAdj = TRUE), "N = ")
  expect_warning(steigerStat(x[1:60, ], dfAdj = TRUE), "N = 60")
})

test_that("dfAdj warning lists every out-of-range factor", {
  w <- tryCatch(steigerStat(make_resid(n = 3000, L = 5), dfAdj = TRUE, tpm = 1),
                warning = conditionMessage)
  expect_match(w, "N = .*L = 5.*tpm = 1")
})

# .checkDfAdjRange -----------------------------------------------------------

test_that(".checkDfAdjRange accepts the calibrated range, including its edges", {
  for (n in c(100, 500, 1500)) for (l in c(10, 30, 60)) for (t in c(-1, -0.5, 0)) {
    expect_no_warning(ok <- .checkDfAdjRange(n, l, t))
    expect_true(ok)
  }
})

test_that(".checkDfAdjRange warns just outside each edge and returns FALSE", {
  cases <- list(
    list(n = 99, l = 30, t = 0, msg = "N = 99 \\(calibrated 100-1500\\)"),
    list(n = 1501, l = 30, t = 0, msg = "N = 1501"),
    list(n = 500, l = 9, t = 0, msg = "L = 9 \\(calibrated 10-60\\)"),
    list(n = 500, l = 61, t = 0, msg = "L = 61"),
    list(n = 500, l = 30, t = -1.01, msg = "tpm = -1.01 \\(calibrated -1 to 0\\)"),
    list(n = 500, l = 30, t = 0.01, msg = "tpm = 0.01")
  )
  for (cs in cases) {
    expect_warning(ok <- .checkDfAdjRange(cs$n, cs$l, cs$t), cs$msg)
    expect_false(ok)
  }
})

test_that(".checkDfAdjRange names only the out-of-range factors", {
  w <- tryCatch(.checkDfAdjRange(500, 5, 0), warning = conditionMessage)
  expect_match(w, "L = 5")
  expect_no_match(w, "N = |tpm = ")
})

test_that(".checkDfAdjRange emits a single warning listing all violations", {
  ws <- character()
  withCallingHandlers(
    .checkDfAdjRange(50, 100, 2),
    warning = function(w) {
      ws <<- c(ws, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(ws, 1)
  expect_match(ws, "N = 50 .*; L = 100 .*; tpm = 2 ")
  expect_match(ws, "Chou & Wang \\(2010\\)")
  expect_match(ws, "extrapolation")
})

test_that(".checkDfAdjRange warning has no call attached", {
  w <- tryCatch(.checkDfAdjRange(50, 30, 0), warning = identity)
  expect_null(conditionCall(w))
})

test_that(".checkDfAdjRange rejects invalid tpm", {
  for (bad in list(NA_real_, NaN, Inf, "0", c(0, -1), numeric(0))) {
    expect_error(.checkDfAdjRange(500, 30, bad), "single finite number")
  }
})

# abilityType ----------------------------------------------------------------

test_that("q3ExpCorrect defaults to ML and treats WLE the same", {
  r <- make_cor(6)
  expect_identical(q3ExpCorrect(r), q3ExpCorrect(r, abilityType = "ML"))
  expect_identical(q3ExpCorrect(r, "WLE"), q3ExpCorrect(r, "ML"))
})

test_that("q3ExpCorrect leaves EAP and PV input unchanged", {
  r <- make_cor(6)
  dimnames(r) <- list(letters[1:6], letters[1:6])
  expect_identical(q3ExpCorrect(r, "EAP"), r)
  expect_identical(q3ExpCorrect(r, "PV"), r)
})

test_that("q3ExpCorrect still validates input when the correction is off", {
  expect_error(q3ExpCorrect(matrix(2, 3, 3), "EAP"), "correlation matrix")
  expect_error(q3ExpCorrect(matrix(1), "PV"), "at least 2 items")
})

test_that("q3ExpCorrect rejects unknown abilityType", {
  expect_error(q3ExpCorrect(make_cor(4), "MLE"), "should be one of")
})

test_that("steigerStat passes abilityType through", {
  x <- make_resid()
  eap <- steigerStat(x, abilityType = "EAP")
  none <- steigerStat(x, q3Adj = FALSE)
  expect_equal(eap$steigerStat, none$steigerStat)
  expect_equal(eap$Cor, none$Cor)
  expect_equal(steigerStat(x)$steigerStat,
               steigerStat(x, abilityType = "WLE")$steigerStat)
  expect_false(isTRUE(all.equal(steigerStat(x)$steigerStat, eap$steigerStat)))
  expect_error(steigerStat(x, abilityType = "foo"), "should be one of")
})
