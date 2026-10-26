make_pv_data <- function(n = 120, M = 5, R = 12, seed = 6151) {
  set.seed(seed)
  df <- data.frame(sex = sample(c("boy", "girl"), n, TRUE), w = runif(n, 1, 3))
  for (i in seq_len(M)) df[[paste0("pv", i)]] <- rnorm(n, 500 + i, 100)
  zone <- rep_len(seq_len(R), n)
  for (r in seq_len(R)) {
    df[[paste0("rw", r)]] <- df$w * ifelse(zone == r, 0, R / (R - 1))
  }
  df
}

test_that("full-sample estimates match base R", {
  df <- make_pv_data()
  pvs <- paste0("pv", 1:5)
  res <- pvMean(df, pvs, weight = "w")
  expect_equal(res$estimate, mean(sapply(pvs, \(p) weighted.mean(df[[p]], df$w))))
  expect_true(is.na(res$samplingVar))
  expect_equal(res$imputationVar, var(sapply(pvs, \(p) weighted.mean(df[[p]], df$w))))

  # unit weights: var matches stats::var, median matches stats::median
  expect_equal(pvVar(df, "pv1")$estimate, var(df$pv1))
  expect_equal(pvMedian(df, "pv1")$estimate, median(df$pv1))
  expect_equal(pvMedian(df[-1, ], "pv1")$estimate, median(df$pv1[-1]))
})

test_that("replicate variance matches a manual calculation", {
  df <- make_pv_data()
  rws <- paste0("rw", 1:12)
  th0 <- weighted.mean(df$pv1, df$w)
  thr <- sapply(rws, \(r) weighted.mean(df$pv1, df[[r]]))
  ss <- sum((thr - th0)^2)
  args <- list(data = df, pvs = "pv1", weight = "w", repWeights = rws)
  expect_equal(do.call(pvMean, c(args, repMethod = "JK1"))$samplingVar, ss * 11 / 12)
  expect_equal(do.call(pvMean, c(args, repMethod = "JK2"))$samplingVar, ss)
  expect_equal(do.call(pvMean, c(args, repMethod = "BRR"))$samplingVar, ss / 12)
  expect_equal(do.call(pvMean, c(args, repMethod = "Fay", fayK = 0.5))$samplingVar, ss / (12 * 0.25))
})

test_that("C++ path agrees with custom R function path", {
  df <- make_pv_data()
  a <- pvSd(df, paste0("pv", 1:5), weight = "w", repWeights = paste0("rw", 1:12), repMethod = "JK1")
  wsd <- function(x, w) {
    k <- w > 0; n <- sum(k); m <- weighted.mean(x, w)
    sqrt(sum(w * (x - m)^2) / sum(w) * n / (n - 1))
  }
  b <- pvStat(df, paste0("pv", 1:5), weight = "w", repWeights = paste0("rw", 1:12),
              repMethod = "JK1", stat = wsd)
  expect_equal(a$estimate, b$estimate)
  expect_equal(a$se, b$se)
})

test_that("by-group results match subsetting", {
  df <- make_pv_data()
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  res <- pvMean(df, pvs, weight = "w", repWeights = rws, repMethod = "JK1", by = "sex")
  expect_equal(res$sex, c("boy", "girl"))
  girls <- pvMean(df[df$sex == "girl", ], pvs, weight = "w", repWeights = rws, repMethod = "JK1")
  expect_equal(res$se[2], girls$se)
  expect_equal(nrow(pvDescribe(df, pvs, weight = "w", by = "sex")), 14)
})

test_that("pvDiff matches manual replicate-level differences", {
  df <- make_pv_data()
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  d <- pvDiff(df, pvs, by = "sex", weight = "w", repWeights = rws,
              repMethod = "JK1", pairs = list(c("girl", "boy")))
  g <- df$sex == "girl"
  est <- sapply(pvs, \(p) sapply(c("w", rws), \(w)
    weighted.mean(df[[p]][g], df[[w]][g]) - weighted.mean(df[[p]][!g], df[[w]][!g])))
  th0 <- est[1, ]
  U <- mean(colSums((est[-1, ] - rep(th0, each = 12))^2) * 11 / 12)
  expect_equal(d$contrast, "girl - boy")
  expect_equal(d$estimate, mean(th0))
  expect_equal(d$se, sqrt(U + 1.2 * var(th0)))
  # default pairs: later minus earlier level
  expect_equal(pvDiff(df, pvs, by = "sex", weight = "w")$contrast, "girl - boy")
  expect_error(pvDiff(df, pvs, by = "sex", pairs = list(c("a", "boy"))), "two levels")
})

test_that("pvIqr and pvDescribe include IQR", {
  df <- make_pv_data()
  q <- pvQuantile(df, "pv1", probs = c(0.25, 0.75), weight = "w")
  expect_equal(pvIqr(df, "pv1", weight = "w")$estimate, diff(q$estimate))
  d <- pvDescribe(df, paste0("pv", 1:5), weight = "w", by = "sex")
  expect_true("iqr" %in% d$stat)
  expect_equal(nrow(d), 14)
})

test_that("pvCor matches manual weighted correlations", {
  df <- make_pv_data()
  df$ses <- df$pv1 / 100 + rnorm(nrow(df))
  df$ses[3] <- NA
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  res <- pvCor(df, pvs, "ses", weight = "w", repWeights = rws, repMethod = "JK1")
  d <- df[!is.na(df$ses), ]
  wcor <- function(x, y, w) cov.wt(cbind(x, y), w, cor = TRUE)$cor[1, 2]
  est <- sapply(pvs, \(p) sapply(c("w", rws), \(w) wcor(d[[p]], d$ses, d[[w]])))
  th0 <- est[1, ]
  U <- mean(colSums((est[-1, ] - rep(th0, each = 12))^2) * 11 / 12)
  expect_equal(res$estimate, mean(th0))
  expect_equal(res$se, sqrt(U + 1.2 * var(th0)))
  expect_equal(res$n, nrow(d))
  expect_equal(nrow(pvCor(df, pvs, c("ses", "w"), by = "sex")), 4)
  expect_error(pvCor(df, pvs, "sex"), "numeric")
})

test_that("pvCor Fisher-z combines on the z scale", {
  df <- make_pv_data()
  df$ses <- df$pv1 / 100 + rnorm(nrow(df))
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  res <- pvCor(df, pvs, "ses", weight = "w", repWeights = rws, repMethod = "JK1",
               combine = "fisherZ")
  wcor <- function(x, y, w) cov.wt(cbind(x, y), w, cor = TRUE)$cor[1, 2]
  z <- atanh(sapply(pvs, \(p) sapply(c("w", rws), \(w) wcor(df[[p]], df$ses, df[[w]]))))
  U <- mean(colSums((z[-1, ] - rep(z[1, ], each = 12))^2) * 11 / 12)
  zSe <- sqrt(U + 1.2 * var(z[1, ]))
  expect_equal(res$zEstimate, mean(z[1, ]))
  expect_equal(res$estimate, tanh(mean(z[1, ])))
  expect_equal(res$zSe, zSe)
  expect_equal(res$ciUpper, tanh(mean(z[1, ]) + qnorm(0.975) * zSe))
  expect_true(res$ciLower < res$estimate && res$estimate < res$ciUpper)
  expect_error(pvCor(transform(df, ses = pv1), pvs[1], "ses", combine = "fisherZ"), "raw")
})

test_that("pvRegression matches lm() and manual replicate SEs", {
  df <- make_pv_data()
  df$ses <- rnorm(nrow(df))
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  res <- pvRegression(df, pvs, ~ ses + sex, weight = "w", repWeights = rws,
                      repMethod = "JK1")
  expect_equal(res$term, c("(Intercept)", "ses", "sexgirl", "R-squared"))
  fits <- sapply(pvs, \(p) sapply(c("w", rws), \(w) {
    d <- df; d$.wt <- df[[w]]
    f <- lm(reformulate(c("ses", "sex"), p), d, weights = .wt)
    coef(f)[["ses"]]
  }))
  U <- mean(colSums((fits[-1, ] - rep(fits[1, ], each = 12))^2) * 11 / 12)
  expect_equal(res$estimate[2], mean(fits[1, ]))
  expect_equal(res$se[2], sqrt(U + 1.2 * var(fits[1, ])))
  r2 <- mean(sapply(pvs, \(p) summary(lm(reformulate(c("ses", "sex"), p), df, weights = w))$r.squared))
  expect_equal(res$estimate[4], r2)
  # by-group fits and singular designs
  expect_equal(nrow(pvRegression(df, pvs, ~ ses, weight = "w", by = "sex")), 6)
  expect_error(pvRegression(transform(df, ses2 = 2 * ses), pvs, ~ ses + ses2), "singular")
  expect_error(pvRegression(df, pvs, pv1 ~ ses), "one-sided")
})

test_that("pvRegression supports coef, vcov, predict, summary, print", {
  df <- make_pv_data()
  df$ses <- rnorm(nrow(df))
  pvs <- paste0("pv", 1:5); rws <- paste0("rw", 1:12)
  res <- pvRegression(df, pvs, ~ ses + sex, weight = "w", repWeights = rws,
                      repMethod = "JK1")
  expect_s3_class(res, "pvRegression")
  b <- coef(res)
  expect_equal(unname(b), res$estimate[1:3])
  expect_equal(names(b), c("(Intercept)", "ses", "sexgirl"))
  expect_equal(unname(sqrt(diag(vcov(res)))), res$se[1:3])
  expect_equal(unname(confint(res)[, 1]), res$ciLower[1:3])
  nd <- data.frame(ses = c(0, 1), sex = c("boy", "girl"))
  expect_equal(unname(predict(res, nd)), c(b[[1]], sum(b)))
  p <- predict(res, nd, se.fit = TRUE)
  expect_equal(p$se.fit[[1]], res$se[1])
  expect_equal(nobs(res), nrow(df))
  s <- summary(res)
  expect_s3_class(s, "summary.pvRegression")
  expect_output(print(s), "R-squared")
  expect_output(print(res), "Coefficients")
  # by-group fits
  rb <- pvRegression(df, pvs, ~ ses, weight = "w", repWeights = rws,
                     repMethod = "JK1", by = "sex")
  expect_named(coef(rb), c("boy", "girl"))
  pb <- predict(rb, data.frame(ses = 1, sex = c("boy", "girl")))
  expect_equal(unname(pb), c(sum(coef(rb)$boy), sum(coef(rb)$girl)))
  expect_output(print(summary(rb)), "sex = girl")
})

test_that("se equals sqrt(U + (1 + 1/M) B) from the returned columns", {
  df <- make_pv_data()
  res <- pvDescribe(df, paste0("pv", 1:5), weight = "w",
                    repWeights = paste0("rw", 1:12), repMethod = "JK1")
  expect_equal(res$se, sqrt(res$samplingVar + (1 + 1 / res$nPVs) * res$imputationVar))
  expect_true(all(res$se > sqrt(res$samplingVar + res$imputationVar)))
})

make_md_data <- function(n = 150, M = 4, R = 10, dims = c("A", "B", "C"), seed = 8821) {
  set.seed(seed)
  df <- data.frame(sex = sample(c("boy", "girl"), n, TRUE), w = runif(n, 1, 3))
  g <- rnorm(n)
  for (d in dims) for (m in seq_len(M)) {
    df[[paste0("pv", m, d)]] <- 0.6 * g + rnorm(n, 0, 0.8)
  }
  zone <- rep_len(seq_len(R), n)
  for (r in seq_len(R)) df[[paste0("rw", r)]] <- df$w * ifelse(zone == r, 0, R / (R - 1))
  df
}

test_that("pvCorMatrix agrees with pvCor on each pair", {
  df <- make_md_data()
  pvl <- lapply(c(a = "A", b = "B", c = "C"), \(d) paste0("pv", 1:4, d))
  rws <- paste0("rw", 1:10)
  res <- pvCorMatrix(df, pvl, weight = "w", repWeights = rws, repMethod = "JK1")
  expect_equal(nrow(res), 3)
  expect_equal(paste(res$dim1, res$dim2), c("a b", "a c", "b c"))

  # Manual: pair PV m of a with PV m of c
  wcor <- function(x, y, w) cov.wt(cbind(x, y), w, cor = TRUE)$cor[1, 2]
  est <- sapply(1:4, \(m) sapply(c("w", rws), \(w) wcor(df[[pvl$a[m]]], df[[pvl$c[m]]], df[[w]])))
  U <- mean(colSums((est[-1, ] - rep(est[1, ], each = 10))^2) * 9 / 10)
  ac <- res[res$dim1 == "a" & res$dim2 == "c", ]
  expect_equal(ac$estimate, mean(est[1, ]))
  expect_equal(ac$se, sqrt(U + 1.25 * var(est[1, ])))

  fz <- pvCorMatrix(df, pvl, weight = "w", repWeights = rws, repMethod = "JK1",
                    combine = "fisherZ")
  expect_equal(fz$zEstimate[2], mean(atanh(est[1, ])))
})

test_that("pvCorMatrix matrix format and by-groups", {
  df <- make_md_data()
  pvl <- list(paste0("pv", 1:4, "A"), paste0("pv", 1:4, "B"), paste0("pv", 1:4, "C"))
  m <- pvCorMatrix(df, pvl, weight = "w", repWeights = paste0("rw", 1:10),
                   repMethod = "JK1", format = "matrix")
  expect_equal(dimnames(m$estimate), list(paste0("dim", 1:3), paste0("dim", 1:3)))
  expect_equal(m$estimate, t(m$estimate))
  expect_equal(unname(diag(m$estimate)), rep(1, 3))
  expect_equal(unname(diag(m$se)), rep(0, 3))
  long <- pvCorMatrix(df, pvl, weight = "w", repWeights = paste0("rw", 1:10), repMethod = "JK1")
  expect_equal(m$estimate["dim3", "dim2"], long$estimate[3])

  byM <- pvCorMatrix(df, pvl, weight = "w", by = "sex", format = "matrix")
  expect_named(byM, c("boy", "girl"))
  byL <- pvCorMatrix(df, pvl, weight = "w", by = "sex")
  expect_equal(byL$sex, rep(c("boy", "girl"), each = 3))
  girls <- pvCorMatrix(df[df$sex == "girl", ], pvl, weight = "w")
  expect_equal(byL$estimate[4:6], girls$estimate)
})

test_that("pvCorMatrix validates input", {
  df <- make_md_data()
  expect_error(pvCorMatrix(df, list(a = paste0("pv", 1:4, "A"))), "at least two")
  expect_error(pvCorMatrix(df, list(a = paste0("pv", 1:4, "A"), b = paste0("pv", 1:3, "B"))),
               "same number of PVs")
  expect_error(pvCorMatrix(df, list(a = "pv1A", b = "pv1A")), "more than once")
  df$pv2B[1] <- NA
  expect_warning(pvCorMatrix(df, list(paste0("pv", 1:4, "A"), paste0("pv", 1:4, "B"))), "1 case")
})

test_that("pvCovMatrix matches cov.wt and pvVar", {
  df <- make_md_data()
  pvl <- lapply(c(a = "A", b = "B", c = "C"), \(d) paste0("pv", 1:4, d))
  rws <- paste0("rw", 1:10)
  res <- pvCovMatrix(df, pvl, weight = "w", repWeights = rws, repMethod = "JK1")
  expect_equal(nrow(res), 6)
  expect_equal(paste(res$dim1, res$dim2), c("a a", "a b", "a c", "b b", "b c", "c c"))

  # Diagonal equals pvVar on that dimension (estimate and SE)
  vb <- pvVar(df, pvl$b, weight = "w", repWeights = rws, repMethod = "JK1")
  bb <- res[res$dim1 == "b" & res$dim2 == "b", ]
  expect_equal(bb$estimate, vb$estimate)
  expect_equal(bb$se, vb$se)

  # Off-diagonal: cov.wt (unbiased with unit weights; scale-invariant n/(n-1) form)
  wcov <- function(x, y, w) {
    k <- w > 0; n <- sum(k); mx <- weighted.mean(x, w); my <- weighted.mean(y, w)
    sum(w * (x - mx) * (y - my)) / sum(w) * n / (n - 1)
  }
  est <- sapply(1:4, \(m) sapply(c("w", rws), \(w) wcov(df[[pvl$a[m]]], df[[pvl$c[m]]], df[[w]])))
  U <- mean(colSums((est[-1, ] - rep(est[1, ], each = 10))^2) * 9 / 10)
  ac <- res[res$dim1 == "a" & res$dim2 == "c", ]
  expect_equal(ac$estimate, mean(est[1, ]))
  expect_equal(ac$se, sqrt(U + 1.25 * var(est[1, ])))
  expect_equal(pvCovMatrix(df, pvl)$estimate[2], mean(sapply(1:4, \(m) cov(df[[pvl$a[m]]], df[[pvl$b[m]]]))))
})

test_that("pvCovMatrix matrix format, single dimension and by-groups", {
  df <- make_md_data()
  pvl <- lapply(c(a = "A", b = "B", c = "C"), \(d) paste0("pv", 1:4, d))
  m <- pvCovMatrix(df, pvl, weight = "w", repWeights = paste0("rw", 1:10),
                   repMethod = "JK1", format = "matrix")
  expect_equal(m$estimate, t(m$estimate))
  expect_true(all(diag(m$se) > 0))
  expect_equal(unname(diag(m$estimate)),
               sapply(pvl, \(p) pvVar(df, p, weight = "w")$estimate), ignore_attr = TRUE)
  # Covariance rescales to the correlation matrix (per PV it is exact; combined it is close)
  r <- pvCorMatrix(df, pvl, weight = "w", format = "matrix")$estimate
  expect_equal(cov2cor(m$estimate), r, tolerance = 0.02)

  one <- pvCovMatrix(df, pvl["a"], weight = "w")
  expect_equal(nrow(one), 1)
  byM <- pvCovMatrix(df, pvl, weight = "w", by = "sex", format = "matrix")
  expect_named(byM, c("boy", "girl"))
  expect_error(pvCovMatrix(df, list()), "at least one")
})

make_mixed_data <- function(n = 160, M = 4, R = 10, seed = 2290) {
  set.seed(seed)
  df <- data.frame(sex = sample(c("boy", "girl"), n, TRUE), w = runif(n, 1, 3),
                   ses = rnorm(n), age = rnorm(n, 60, 5))
  g <- 0.5 * df$ses + rnorm(n)
  for (d in c("A", "B")) for (m in seq_len(M)) {
    df[[paste0("pv", m, d)]] <- 0.6 * g + rnorm(n, 0, 0.8)
  }
  zone <- rep_len(seq_len(R), n)
  for (r in seq_len(R)) df[[paste0("rw", r)]] <- df$w * ifelse(zone == r, 0, R / (R - 1))
  df
}

test_that("mixed latent/manifest matrices reproduce the pairwise results", {
  df <- make_mixed_data()
  rws <- paste0("rw", 1:10)
  meas <- list(A = paste0("pv", 1:4, "A"), B = paste0("pv", 1:4, "B"),
               ses = "ses", age = "age")
  res <- pvCorMatrix(df, meas, weight = "w", repWeights = rws, repMethod = "JK1")
  expect_equal(nrow(res), 6)

  # latent-latent equals the all-latent result
  ab <- pvCorMatrix(df, meas[c("A", "B")], weight = "w", repWeights = rws, repMethod = "JK1")
  expect_equal(res[res$dim1 == "A" & res$dim2 == "B", "se"], ab$se)

  # latent-manifest equals the standard method (repeat the manifest per PV)
  wcor <- function(x, y, w) cov.wt(cbind(x, y), w, cor = TRUE)$cor[1, 2]
  est <- sapply(1:4, \(m) sapply(c("w", rws), \(w) wcor(df[[meas$A[m]]], df$ses, df[[w]])))
  U <- mean(colSums((est[-1, ] - rep(est[1, ], each = 10))^2) * 9 / 10)
  as <- res[res$dim1 == "A" & res$dim2 == "ses", ]
  expect_equal(as$estimate, mean(est[1, ]))
  expect_equal(as$se, sqrt(U + 1.25 * var(est[1, ])))
  expect_equal(as$nPVs, 4)

  # manifest-manifest: ordinary weighted correlation, B = 0, nPVs = 1
  sa <- res[res$dim1 == "ses" & res$dim2 == "age", ]
  e <- sapply(c("w", rws), \(w) wcor(df$ses, df$age, df[[w]]))
  expect_equal(sa$estimate, unname(e[1]))
  expect_equal(sa$imputationVar, 0)
  expect_equal(sa$nPVs, 1)
  expect_equal(sa$se, sqrt(sum((e[-1] - e[1])^2) * 9 / 10))

  # pvCor gives the same numbers, with a list of mixed vars
  pc <- pvCor(df, meas$A, list(B = meas$B, ses = "ses"), weight = "w",
              repWeights = rws, repMethod = "JK1")
  expect_equal(pc$variable, c("B", "ses"))
  expect_equal(pc$se[2], as$se)
})

test_that("all-manifest input gives the ordinary weighted matrices", {
  df <- make_mixed_data()
  cv <- pvCovMatrix(df, list("ses", "age"), weight = "w", format = "matrix")
  expect_equal(dimnames(cv$estimate)[[1]], c("ses", "age"))
  n <- nrow(df)
  ref <- cov.wt(df[c("ses", "age")], df$w, method = "ML")$cov * n / (n - 1)
  expect_equal(cv$estimate, ref, ignore_attr = TRUE)
  expect_false(any(cv$latent))
  cr <- pvCorMatrix(df, list("ses", "age"), weight = "w")
  expect_equal(cr$imputationVar, 0)
  expect_equal(cr$estimate, cov2cor(ref)[2, 1])
  # pvCor with manifest pvs works too
  expect_equal(pvCor(df, "ses", "age", weight = "w")$estimate, cr$estimate)
})

test_that("mixed inputs: edge cases", {
  df <- make_mixed_data()
  A <- paste0("pv", 1:4, "A")
  # Unequal latent PV counts
  expect_error(pvCorMatrix(df, list(A = A, B = paste0("pv", 1:3, "B"))), "same number of PVs")
  # Non-numeric
  expect_error(pvCorMatrix(df, list(A = A, sex = "sex")), "numeric")
  # Duplicate column across measures
  expect_error(pvCorMatrix(df, list(A = A, x = "pv1A")), "more than once")
  # Duplicate names
  expect_error(pvCorMatrix(df, list(ses = A, ses = "ses")), "unique")
  # Constant within group -> NA and warning
  df$girl <- as.numeric(df$sex == "girl")
  expect_warning(r <- pvCorMatrix(df, list(A = A, girl = "girl"), by = "sex"), "constant")
  expect_true(all(is.na(r$estimate)))
  # Pairwise deletion in pvCor; listwise in pvCorMatrix
  df$ses[1:5] <- NA
  pc <- pvCor(df, A, c("ses", "age"))
  expect_equal(pc$n, c(nrow(df) - 5, nrow(df)))
  expect_warning(pm <- pvCorMatrix(df, list(A = A, ses = "ses", age = "age")), "5 case")
  expect_equal(unique(pm$n), nrow(df) - 5)
  # Unnamed measures
  m <- pvCorMatrix(df, list(A, "age"), format = "matrix")
  expect_equal(rownames(m$estimate), c("dim1", "age"))
  expect_equal(m$latent, c(dim1 = TRUE, age = FALSE))
})

test_that("Fisher-z matrix warns when not positive semi-definite", {
  # Draw 1 of L tracks y, draw 2 tracks z, with y and z uncorrelated. Each
  # per-draw matrix is valid, but averaging r ~ 1 and r ~ 0 on the z scale
  # pulls both L-y and L-z towards 1, which is impossible when y-z ~ 0.
  set.seed(6034)
  n <- 200
  df <- data.frame(y = rnorm(n), z = rnorm(n))
  df$pv1 <- df$y + rnorm(n, 0, 0.03)
  df$pv2 <- df$z + rnorm(n, 0, 0.03)
  meas <- list(L = c("pv1", "pv2"), y = "y", z = "z")
  expect_warning(
    fz <- pvCorMatrix(df, meas, combine = "fisherZ", format = "matrix"),
    "not positive semi-definite"
  )
  expect_lt(min(eigen(fz$estimate, only.values = TRUE)$values), 0)
  expect_no_warning(raw <- pvCorMatrix(df, meas, format = "matrix"))
  expect_gte(min(eigen(raw$estimate, only.values = TRUE)$values), 0)
  df$grp <- rep(c("a", "b"), each = n / 2)
  expect_warning(pvCorMatrix(df, meas, combine = "fisherZ", by = "grp"), "group\\(s\\)")
})

test_that("mixed covariance matrix is positive semi-definite", {
  df <- make_mixed_data()
  meas <- list(A = paste0("pv", 1:4, "A"), B = paste0("pv", 1:4, "B"), ses = "ses", age = "age")
  cv <- pvCovMatrix(df, meas, weight = "w", format = "matrix")
  expect_gte(min(eigen(cv$estimate, only.values = TRUE)$values), 0)
  expect_equal(cv$estimate["age", "age"], pvVar(df, "age", weight = "w")$estimate)
})

test_that("missing data and bad inputs are handled", {
  df <- make_pv_data()
  df$pv1[1] <- NA
  expect_warning(pvMean(df, "pv1"), "1 case")
  expect_error(pvMean(df, "nope"), "not found")
  expect_error(pvStat(df, "pv2", stat = "mode"), "unknown stat")
})
