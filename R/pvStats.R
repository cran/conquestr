.pvStatCodes <- c(mean = 1L, var = 2L, sd = 3L, quantile = 4L, sumw = 5L, iqr = 6L)

#' @title pvStat
#'
#' @description Core function for estimating population statistics from
#'   plausible values (PVs) with replicate-weight standard errors. The
#'   statistic is computed for each PV under the full-sample weight and under
#'   each replicate weight. Results are combined using Rubin's rules:
#'
#'   * estimate: mean of the M full-sample PV estimates;
#'   * sampling variance (U): mean over PVs of the replicate variance;
#'   * imputation variance (B): variance of the M full-sample PV estimates;
#'   * total error variance: U + (1 + 1/M) B.
#'
#'   See the "Variance components" section for how the returned columns
#'   relate to `se`.
#'
#'   Replicate variance for one PV is `scale * sum((theta_r - theta_0)^2)`,
#'   where `scale` depends on `repMethod`:
#'
#'   | repMethod | scale |
#'   |-----------|-------|
#'   | "BRR"     | 1 / R |
#'   | "Fay"     | 1 / (R (1 - k)^2), Fay's variant of BRR with factor `fayK` |
#'   | "JK1"     | (R - 1) / R, delete-one jackknife |
#'   | "JK2"     | 1, paired jackknife (e.g., TIMSS, PIRLS) |
#'
#' @param data A data frame.
#' @param pvs Character vector of PV column names (one dimension).
#' @param weight Name of the full-sample weight column. If `NULL`, unit
#'   weights are used.
#' @param repWeights Character vector of replicate weight column names. If
#'   `NULL`, only the imputation variance is estimated and the sampling
#'   variance is `NA`.
#' @param repMethod One of "BRR", "Fay", "JK1", "JK2".
#' @param stat One of "mean", "var", "sd", "median", "quantile", "iqr", "sumw", or a
#'   function `function(x, w)` returning a single number (evaluated in R,
#'   slower).
#' @param by Optional name of a grouping column (e.g., sex). Statistics are
#'   computed within each level; an "All" row is not added.
#' @param probs Probability for `stat = "quantile"`.
#' @param fayK Fay's factor `k` for `repMethod = "Fay"`.
#' @param repScale Optional numeric override for the replicate scale factor.
#' @return A data frame with one row per group: `group`, `stat`, `estimate`,
#'   `se`, `samplingVar`, `imputationVar`, `nPVs`, `nReps`, `n`.
#'   `samplingVar` is U and `imputationVar` is B, the between-PV variance
#'   *before* Rubin's (1 + 1/M) correction, so
#'   `se^2 = samplingVar + (1 + 1/nPVs) * imputationVar`, not
#'   `samplingVar + imputationVar`. See "Variance components".
#'
#' @section Variance components:
#'   The error variance of a PV-based estimate has two sources: sampling
#'   (which students were sampled) and measurement (each student's
#'   proficiency is uncertain, which is represented by drawing M PVs). The
#'   returned columns are:
#'
#'   | column | symbol | definition |
#'   |--------|--------|------------|
#'   | `samplingVar` | U | mean over PVs of the replicate sampling variance |
#'   | `imputationVar` | B | variance of the M full-sample PV estimates, `var(theta_m)` (divisor M - 1), *before* Rubin's correction |
#'   | `se` | | `sqrt(U + (1 + 1/M) * B)` |
#'
#'   **Why (1 + 1/M) B rather than B.** B estimates how much the statistic
#'   varies from one PV to another. The reported estimate, however, is the
#'   *average* of only M PV estimates, not the value that would be obtained
#'   with infinitely many PVs. That average has its own simulation error,
#'   with variance B / M, which must be added to the total. Hence the
#'   measurement component is B + B/M = (1 + 1/M) B (Rubin, 1987). Put
#'   another way, U + B follows from the law of total variance; the
#'   additional B/M term corrects for using a finite number of PVs. The
#'   correction matters most when M is small: it adds 20% to B with 5 PVs
#'   and 10% with 10 PVs.
#'
#'   **Recovering `se` from the columns.**
#'
#'   ```
#'   res <- pvMean(...)
#'   all.equal(res$se,
#'             sqrt(res$samplingVar + (1 + 1 / res$nPVs) * res$imputationVar))
#'   ```
#'
#'   Computing `sqrt(samplingVar + imputationVar)` omits the B/M term and so
#'   slightly understates the standard error. The share of error variance
#'   due to measurement is `(1 + 1/M) * B / se^2`.
#'
#'   If `repWeights` is `NULL`, `samplingVar` and therefore `se` are `NA`;
#'   `imputationVar` is still returned.
#'
#'   Reference: Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in
#'   Surveys*. Wiley.
#' @examples
#' set.seed(2718)
#' n <- 200
#' df <- data.frame(sex = sample(c("boy", "girl"), n, TRUE), w = runif(n, 1, 3))
#' for (i in 1:5) df[[paste0("pv", i)]] <- rnorm(n)
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' pvStat(df, paste0("pv", 1:5), "w", paste0("rw", 1:20), "JK1", "mean", by = "sex")
pvStat <- function(data, pvs, weight = NULL, repWeights = NULL,
                   repMethod = c("BRR", "Fay", "JK1", "JK2"),
                   stat = "mean", by = NULL, probs = 0.5, fayK = 0.5,
                   repScale = NULL) {
  repMethod <- match.arg(repMethod)
  stopifnot(is.data.frame(data), is.character(pvs), length(pvs) >= 1)
  cols <- c(pvs, weight, repWeights, by)
  missingCols <- setdiff(cols, names(data))
  if (length(missingCols) > 0) {
    stop("columns not found in data: ", paste(missingCols, collapse = ", "))
  }

  statName <- if (is.function(stat)) "custom" else stat
  if (is.character(stat)) {
    if (stat == "median") { stat <- "quantile"; probs <- 0.5 }
    if (!stat %in% names(.pvStatCodes)) stop("unknown stat: ", stat)
    if (stat == "quantile") statName <- paste0("q", format(probs))
  }
  if (length(probs) != 1) stop("probs must be length 1; use pvQuantile() for several")

  nReps <- length(repWeights)
  scale <- .pvRepScale(repMethod, nReps, fayK, repScale)

  # Listwise deletion on analysis columns
  keep <- stats::complete.cases(data[, cols, drop = FALSE])
  if (any(!keep)) {
    warning(sum(!keep), " case(s) with missing values dropped")
    data <- data[keep, , drop = FALSE]
  }

  X <- as.matrix(data[, pvs, drop = FALSE])
  storage.mode(X) <- "double"
  w0 <- if (is.null(weight)) rep(1, nrow(data)) else as.numeric(data[[weight]])
  W <- cbind(w0, as.matrix(data[, repWeights, drop = FALSE]))
  storage.mode(W) <- "double"

  groups <- if (is.null(by)) factor(rep("All", nrow(data))) else as.factor(data[[by]])
  groups <- droplevels(groups)
  ests <- list()
  res <- lapply(levels(groups), function(g) {
    idx <- which(groups == g)
    est <- .pvRepEstimates(X[idx, , drop = FALSE], W[idx, , drop = FALSE], stat, probs)
    ests[[g]] <<- est
    out <- .pvCombine(est, scale)
    data.frame(group = g, stat = statName, out, nPVs = ncol(X), nReps = nReps,
               n = length(idx), row.names = NULL)
  })
  res <- do.call(rbind, res)
  if (!is.null(by)) names(res)[1] <- by
  # Per-group M x (R + 1) estimate matrices and scale, used by pvDiff()
  attr(res, "estimates") <- ests
  attr(res, "repScale") <- scale
  res
}

#' @title pvDiff
#'
#' @description Differences in a statistic between levels of a grouping
#'   variable (e.g., girls minus boys), with standard errors that account for
#'   both sampling (replicate) and imputation (PV) variance. The difference is
#'   computed within each PV and each replicate weight, then combined with
#'   Rubin's rules, so the covariance between groups induced by the sampling
#'   design is accounted for.
#'
#' @inheritParams pvStat
#' @param by Name of the grouping column (required).
#' @param pairs Optional list of length-2 character vectors, each `c(a, b)`
#'   giving the contrast `a - b`. Defaults to all pairwise contrasts, later
#'   level minus earlier level.
#' @param ... Further arguments passed to [pvStat()] (e.g., `weight`,
#'   `repWeights`, `repMethod`, `stat`, `probs`).
#' @return A data frame with `contrast`, `stat`, `estimate`, `se`,
#'   `samplingVar`, `imputationVar`, `t` (estimate / se). `imputationVar` is
#'   B before Rubin's correction; `se^2 = samplingVar + (1 + 1/M) *
#'   imputationVar`, where M is the number of PVs. See the "Variance
#'   components" section of [pvStat()].
#' @examples
#' set.seed(2718)
#' n <- 200
#' df <- data.frame(sex = sample(c("boy", "girl"), n, TRUE), w = runif(n, 1, 3))
#' for (i in 1:5) df[[paste0("pv", i)]] <- rnorm(n)
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' pvDiff(df, paste0("pv", 1:5), by = "sex", weight = "w",
#'        repWeights = paste0("rw", 1:20), repMethod = "JK1",
#'        pairs = list(c("girl", "boy")))
pvDiff <- function(data, pvs, by, pairs = NULL, ...) {
  if (missing(by) || is.null(by)) stop("`by` is required")
  grp <- pvStat(data, pvs, by = by, ...)
  ests <- attr(grp, "estimates")
  scale <- attr(grp, "repScale")
  lv <- names(ests)
  if (is.null(pairs)) {
    if (length(lv) < 2) stop("`by` must have at least two levels")
    pairs <- lapply(utils::combn(lv, 2, simplify = FALSE), rev)
  }
  res <- lapply(pairs, function(p) {
    p <- as.character(p)
    if (length(p) != 2 || !all(p %in% lv)) {
      stop("each element of `pairs` must be two levels of `", by, "`: ",
           paste(lv, collapse = ", "))
    }
    out <- .pvCombine(ests[[p[1]]] - ests[[p[2]]], scale)
    data.frame(contrast = paste(p[1], "-", p[2]), stat = grp$stat[1], out,
               t = out$estimate / out$se, row.names = NULL)
  })
  do.call(rbind, res)
}

.pvRepScale <- function(repMethod, nReps, fayK, repScale) {
  if (!is.null(repScale)) return(repScale)
  if (nReps == 0) return(NA_real_)
  switch(repMethod,
    BRR = 1 / nReps,
    Fay = 1 / (nReps * (1 - fayK)^2),
    JK1 = (nReps - 1) / nReps,
    JK2 = 1
  )
}

# Returns an M x (R + 1) matrix of estimates
.pvRepEstimates <- function(X, W, stat, probs) {
  if (is.function(stat)) {
    out <- matrix(NA_real_, ncol(X), ncol(W))
    for (m in seq_len(ncol(X))) for (r in seq_len(ncol(W))) {
      out[m, r] <- stat(X[, m], W[, r])
    }
    return(out)
  }
  pvRepStatCpp(X, W, .pvStatCodes[[stat]], probs)
}

# Rubin's rules combination of an M x (R + 1) estimate matrix.
# Returns B uncorrected as imputationVar; the (1 + 1/M) factor is applied only
# in se. Documented in the "Variance components" section of ?pvStat.
.pvCombine <- function(est, scale) {
  theta <- est[, 1]
  M <- length(theta)
  U <- if (ncol(est) > 1) {
    mean(scale * rowSums((est[, -1, drop = FALSE] - theta)^2))
  } else NA_real_
  B <- if (M > 1) stats::var(theta) else 0
  totalVar <- U + (1 + 1 / M) * B
  data.frame(estimate = mean(theta), se = sqrt(totalVar),
             samplingVar = U, imputationVar = B)
}

#' @title pvMean
#' @description Mean of plausible values with replicate SEs. See [pvStat()].
#' @inheritParams pvStat
#' @param ... Further arguments passed to [pvStat()].
pvMean <- function(data, pvs, ...) pvStat(data, pvs, stat = "mean", ...)

#' @title pvVar
#' @description Variance of plausible values with replicate SEs. See [pvStat()].
#' @inheritParams pvMean
pvVar <- function(data, pvs, ...) pvStat(data, pvs, stat = "var", ...)

#' @title pvSd
#' @description Standard deviation of plausible values with replicate SEs. See [pvStat()].
#' @inheritParams pvMean
pvSd <- function(data, pvs, ...) pvStat(data, pvs, stat = "sd", ...)

#' @title pvMedian
#' @description Median of plausible values with replicate SEs. See [pvStat()].
#' @inheritParams pvMean
pvMedian <- function(data, pvs, ...) pvStat(data, pvs, stat = "median", ...)

#' @title pvQuantile
#' @description Quantiles of plausible values with replicate SEs. See [pvStat()].
#' @inheritParams pvMean
#' @param probs Numeric vector of probabilities.
pvQuantile <- function(data, pvs, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), ...) {
  do.call(rbind, lapply(probs, function(p) {
    pvStat(data, pvs, stat = "quantile", probs = p, ...)
  }))
}

#' @title pvIqr
#' @description Interquartile range (weighted Q3 - Q1) of plausible values
#'   with replicate SEs. See [pvStat()].
#' @inheritParams pvMean
pvIqr <- function(data, pvs, ...) pvStat(data, pvs, stat = "iqr", ...)

#' @title pvDescribe
#' @description Standard descriptive statistics (mean, SD, variance,
#'   quartiles, IQR) of plausible values with replicate SEs, optionally by group.
#' @inheritParams pvMean
#' @return A data frame stacking the results of [pvStat()] for each statistic.
#'   `imputationVar` is the between-PV variance B before Rubin's correction,
#'   so `se^2 = samplingVar + (1 + 1/nPVs) * imputationVar`. See the
#'   "Variance components" section of [pvStat()].
pvDescribe <- function(data, pvs, ...) {
  rbind(
    pvMean(data, pvs, ...),
    pvSd(data, pvs, ...),
    pvVar(data, pvs, ...),
    pvQuantile(data, pvs, probs = c(0.25, 0.5, 0.75), ...),
    pvIqr(data, pvs, ...)
  )
}

#' @title pvCor
#'
#' @description Weighted Pearson correlations between one measure and one or
#'   more other measures, with standard errors from replicate weights and
#'   PVs. Each measure may be latent (a set of M plausible values) or
#'   manifest (a single observed column). For each PV and each weight the
#'   correlation is computed, then combined with Rubin's rules as in
#'   [pvStat()]. See the "Latent and manifest measures" section of
#'   [pvCorMatrix()] for how the two kinds are combined and for important
#'   caveats (in particular, conditioning).
#'
#'   With `combine = "raw"` (default) correlations are combined on the r
#'   scale, as in the PISA and TIMSS technical reports. With
#'   `combine = "fisherZ"` each PV/replicate correlation is transformed with
#'   `atanh()`, the variance components are computed on the z scale, and the
#'   estimate and confidence limits are back-transformed with `tanh()`. The
#'   reported `se` is then the delta-method SE on the r scale,
#'   `zSe * (1 - r^2)`, and the variance components are on the z scale. The
#'   z scale is preferable when correlations are near +/-1, where the r scale
#'   is bounded and its sampling distribution is skewed.
#'
#' @param combine Either "raw" or "fisherZ".
#' @param confLevel Confidence level for `ciLower`/`ciUpper` (normal
#'   approximation on the combining scale).
#'
#' @inheritParams pvStat
#' @param pvs Character vector of column names for the focal measure: its PV
#'   columns in draw order (latent), or a single column (manifest).
#' @param vars The measures to correlate with `pvs`. Either a character
#'   vector of manifest (observed) column names, or a list whose elements
#'   are character vectors: length 1 for a manifest variable, length M for a
#'   latent measure (PV columns in draw order). List element names are used
#'   in the `variable` column; unnamed manifest elements are named by their
#'   column and unnamed latent elements `var1`, `var2`, ... Every latent
#'   measure (including `pvs`) must have the same number of PVs.
#' @param by Optional name of a grouping column; correlations are computed
#'   within each level.
#' @return A data frame with one row per variable (and group): `variable`,
#'   `estimate`, `se`, `samplingVar`, `imputationVar`, `ciLower`, `ciUpper`,
#'   `combine`, `nPVs`, `nReps`, `n`; with `fisherZ`, also `zEstimate` and
#'   `zSe`. `nPVs` is 1 when both measures are manifest (the estimate is then
#'   an ordinary weighted correlation and `imputationVar` is 0).
#'   `imputationVar` is B before Rubin's correction (see the "Variance
#'   components" section of [pvStat()]). With `combine = "raw"`,
#'   `se^2 = samplingVar + (1 + 1/nPVs) * imputationVar`. With
#'   `combine = "fisherZ"` the variance components are on the z scale, so the
#'   same identity holds for `zSe`, and `se = zSe * (1 - estimate^2)`.
#'   Missing values are deleted pairwise per variable (only cases missing on
#'   `pvs`, the weights, `by`, or that variable are dropped). This is safe
#'   because each row is a separate correlation; use [pvCorMatrix()] for a
#'   matrix, which requires a common set of cases.
#' @examples
#' set.seed(2718)
#' n <- 200
#' df <- data.frame(ses = rnorm(n), w = runif(n, 1, 3))
#' for (i in 1:5) df[[paste0("pv", i)]] <- 0.4 * df$ses + rnorm(n)
#' for (i in 1:5) df[[paste0("pvB", i)]] <- 0.5 * df[[paste0("pv", i)]] + rnorm(n)
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' # PVs with a manifest variable
#' pvCor(df, paste0("pv", 1:5), "ses", weight = "w",
#'       repWeights = paste0("rw", 1:20), repMethod = "JK1")
#' # PVs with a manifest variable and another latent measure
#' pvCor(df, paste0("pv", 1:5), list(ses = "ses", B = paste0("pvB", 1:5)),
#'       weight = "w", repWeights = paste0("rw", 1:20), repMethod = "JK1")
pvCor <- function(data, pvs, vars, weight = NULL, repWeights = NULL,
                  repMethod = c("BRR", "Fay", "JK1", "JK2"), by = NULL,
                  fayK = 0.5, repScale = NULL,
                  combine = c("raw", "fisherZ"), confLevel = 0.95) {
  repMethod <- match.arg(repMethod)
  combine <- match.arg(combine)
  stopifnot(is.data.frame(data), is.character(pvs), length(pvs) >= 1)
  if (is.character(vars)) vars <- stats::setNames(as.list(vars), vars)
  if (!is.list(vars) || length(vars) < 1 ||
      !all(vapply(vars, function(v) is.character(v) && length(v) >= 1, logical(1)))) {
    stop("`vars` must be a character vector or a list of character vectors")
  }
  vars <- .pvNameMeasures(vars, prefix = "var")
  focal <- ".focal"
  if (focal %in% names(vars)) stop("`vars` may not contain an element named '.focal'")

  res <- lapply(names(vars), function(v) {
    meas <- stats::setNames(list(pvs, vars[[v]]), c(focal, v))
    out <- .pvMatrixEngine(data, meas, weight, repWeights, repMethod, by, fayK,
                           repScale, combine, confLevel, format = "long",
                           type = "cor", warnDropped = FALSE)
    keepCols <- setdiff(names(out), c(by, "dim1", "dim2"))
    res <- data.frame(variable = v, out[keepCols], row.names = NULL)
    if (!is.null(by)) res <- cbind(out[by], res)
    res
  })
  do.call(rbind, res)
}

.pvCorCombine <- function(est, scale, combine, zCrit) {
  if (combine == "raw") {
    out <- .pvCombine(est, scale)
    out$ciLower <- out$estimate - zCrit * out$se
    out$ciUpper <- out$estimate + zCrit * out$se
    return(out)
  }
  if (any(abs(est) >= 1, na.rm = TRUE)) {
    stop("correlations of +/-1 cannot be Fisher-z transformed; use combine = \"raw\"")
  }
  z <- .pvCombine(atanh(est), scale)
  r <- tanh(z$estimate)
  data.frame(estimate = r, se = z$se * (1 - r^2),
             samplingVar = z$samplingVar, imputationVar = z$imputationVar,
             ciLower = tanh(z$estimate - zCrit * z$se),
             ciUpper = tanh(z$estimate + zCrit * z$se),
             zEstimate = z$estimate, zSe = z$se)
}

#' @title pvRegression
#'
#' @description Weighted linear regression of plausible values on background
#'   variables, with standard errors from replicate weights and PVs. For each
#'   PV and each weight (full-sample plus replicates) the model is fitted by
#'   weighted least squares; coefficients and R-squared are then combined
#'   with Rubin's rules as in [pvStat()]. The heavy lifting is done in C++
#'   (normal equations factored once per weight and reused across PVs).
#'
#' @inheritParams pvStat
#' @param formula A one-sided formula of predictors, e.g. `~ ses + sex`.
#'   Factors, interactions and transformations are expanded by
#'   [stats::model.matrix()]. The PVs are the outcome.
#' @param by Optional name of a grouping column; a separate model is fitted
#'   within each level.
#' @param confLevel Confidence level for `ciLower`/`ciUpper` (normal
#'   approximation).
#' @return A data frame with one row per term (and group), including
#'   `"R-squared"`: `term`, `estimate`, `se`, `t`, `ciLower`, `ciUpper`,
#'   `samplingVar`, `imputationVar`, `nPVs`, `nReps`, `n`. `imputationVar`
#'   is B before Rubin's correction, so
#'   `se^2 = samplingVar + (1 + 1/nPVs) * imputationVar` (see the "Variance
#'   components" section of [pvStat()]). The R-squared row
#'   is the PV-averaged weighted R-squared; treat its SE as approximate since
#'   R-squared is bounded.
#' @examples
#' set.seed(2718)
#' n <- 200
#' df <- data.frame(ses = rnorm(n), sex = sample(c("boy", "girl"), n, TRUE),
#'                  w = runif(n, 1, 3))
#' for (i in 1:5) df[[paste0("pv", i)]] <- 0.4 * df$ses + rnorm(n)
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' pvRegression(df, paste0("pv", 1:5), ~ ses + sex, weight = "w",
#'              repWeights = paste0("rw", 1:20), repMethod = "JK1")
pvRegression <- function(data, pvs, formula, weight = NULL, repWeights = NULL,
                         repMethod = c("BRR", "Fay", "JK1", "JK2"), by = NULL,
                         fayK = 0.5, repScale = NULL, confLevel = 0.95) {
  repMethod <- match.arg(repMethod)
  stopifnot(is.data.frame(data), is.character(pvs), length(pvs) >= 1,
            inherits(formula, "formula"))
  if (length(formula) != 2) stop("`formula` must be one-sided, e.g. ~ ses + sex")
  predVars <- all.vars(formula)
  cols <- c(pvs, weight, repWeights, by, predVars)
  missingCols <- setdiff(cols, names(data))
  if (length(missingCols) > 0) {
    stop("columns not found in data: ", paste(missingCols, collapse = ", "))
  }
  keep <- stats::complete.cases(data[, cols, drop = FALSE])
  if (any(!keep)) {
    warning(sum(!keep), " case(s) with missing values dropped")
    data <- data[keep, , drop = FALSE]
  }
  nReps <- length(repWeights)
  scale <- .pvRepScale(repMethod, nReps, fayK, repScale)
  zCrit <- stats::qnorm(1 - (1 - confLevel) / 2)

  Y <- as.matrix(data[, pvs, drop = FALSE]); storage.mode(Y) <- "double"
  w0 <- if (is.null(weight)) rep(1, nrow(data)) else as.numeric(data[[weight]])
  W <- cbind(w0, as.matrix(data[, repWeights, drop = FALSE])); storage.mode(W) <- "double"
  groups <- if (is.null(by)) factor(rep("All", nrow(data))) else droplevels(as.factor(data[[by]]))

  fits <- list()
  res <- lapply(levels(groups), function(g) {
    idx <- which(groups == g)
    mf <- stats::model.frame(formula, data[idx, , drop = FALSE])
    X <- stats::model.matrix(attr(mf, "terms"), mf)
    terms <- c(colnames(X), "R-squared")
    est <- pvRepRegCpp(X, Y[idx, , drop = FALSE], W[idx, , drop = FALSE])
    if (anyNA(est[seq_len(ncol(X)), , 1])) {
      stop("design matrix is singular", if (!is.null(by)) paste0(" in group '", g, "'"))
    }
    if (anyNA(est)) warning("some replicate fits were singular; their estimates are NA")
    out <- do.call(rbind, lapply(seq_along(terms), function(j) {
      .pvCombine(matrix(est[j, , ], nrow = dim(est)[2]), scale)
    }))
    beta <- stats::setNames(out$estimate[seq_len(ncol(X))], colnames(X))
    fits[[g]] <<- list(
      coefficients = beta,
      vcov = .pvRegVcov(est[seq_len(ncol(X)), , , drop = FALSE], scale, colnames(X)),
      r.squared = out$estimate[ncol(X) + 1],
      terms = attr(mf, "terms"),
      xlevels = stats::.getXlevels(attr(mf, "terms"), mf),
      contrasts = attr(X, "contrasts"),
      n = length(idx)
    )
    data.frame(group = g, term = terms, estimate = out$estimate, se = out$se,
               t = out$estimate / out$se,
               ciLower = out$estimate - zCrit * out$se,
               ciUpper = out$estimate + zCrit * out$se,
               samplingVar = out$samplingVar, imputationVar = out$imputationVar,
               nPVs = ncol(Y), nReps = nReps, n = length(idx), row.names = NULL)
  })
  res <- do.call(rbind, res)
  if (is.null(by)) res$group <- NULL else names(res)[1] <- by
  attr(res, "fits") <- fits
  attr(res, "call") <- match.call()
  attr(res, "by") <- by
  attr(res, "confLevel") <- confLevel
  attr(res, "repMethod") <- if (nReps > 0) repMethod else NA_character_
  class(res) <- c("pvRegression", "data.frame")
  res
}

# Rubin-combined covariance matrix of coefficients from a p x M x (R + 1) array
.pvRegVcov <- function(est, scale, nms) {
  p <- dim(est)[1]; M <- dim(est)[2]; R1 <- dim(est)[3]
  theta <- matrix(est[, , 1], p, M)
  U <- if (R1 > 1) {
    Reduce(`+`, lapply(seq_len(M), function(m) {
      d <- matrix(est[, m, -1], p, R1 - 1) - theta[, m]
      scale * tcrossprod(d)
    })) / M
  } else matrix(NA_real_, p, p)
  B <- if (M > 1) stats::cov(t(theta)) else matrix(0, p, p)
  V <- U + (1 + 1 / M) * B
  dimnames(V) <- list(nms, nms)
  V
}

# Fits from a pvRegression object; drops to the plain data frame if the
# attributes were lost (e.g., after subsetting)
.pvRegFits <- function(object) {
  fits <- attr(object, "fits")
  if (is.null(fits)) stop("model information missing; was the object subset or modified?")
  fits
}

.pvRegSimplify <- function(x, object) {
  if (length(x) == 1 && is.null(attr(object, "by"))) x[[1]] else x
}

#' Methods for pvRegression objects
#'
#' Standard model methods for objects returned by [pvRegression()]. When the
#' model was fitted with `by`, `coef()`, `vcov()` and `confint()` return a
#' list with one element per group, and `predict()` uses the group-specific
#' model for each row of `newdata` (which must then contain the `by` column).
#' Inference uses the normal approximation, as in [pvRegression()].
#'
#' @param object,x A `pvRegression` object.
#' @param newdata Data frame of predictor values.
#' @param se.fit Logical; return standard errors of the predictions,
#'   `sqrt(diag(X V X'))` with V the Rubin-combined covariance from [vcov()].
#' @param parm Terms to include (names or indices).
#' @param level Confidence level.
#' @param digits Number of significant digits to print.
#' @param ... Unused.
#' @importFrom stats coef vcov confint nobs predict
#' @name pvRegression-methods
NULL

#' @rdname pvRegression-methods
#' @export
coef.pvRegression <- function(object, ...) {
  .pvRegSimplify(lapply(.pvRegFits(object), `[[`, "coefficients"), object)
}

#' @rdname pvRegression-methods
#' @export
vcov.pvRegression <- function(object, ...) {
  .pvRegSimplify(lapply(.pvRegFits(object), `[[`, "vcov"), object)
}

#' @rdname pvRegression-methods
#' @export
confint.pvRegression <- function(object, parm, level = 0.95, ...) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (missing(parm)) parm <- NULL
  out <- lapply(.pvRegFits(object), function(f) {
    b <- f$coefficients
    se <- sqrt(diag(f$vcov))
    ci <- cbind(b - z * se, b + z * se)
    pct <- paste(format(100 * c((1 - level) / 2, 1 - (1 - level) / 2),
                        trim = TRUE, scientific = FALSE, digits = 3), "%")
    dimnames(ci) <- list(names(b), pct)
    if (is.null(parm)) ci else ci[parm, , drop = FALSE]
  })
  .pvRegSimplify(out, object)
}

#' @rdname pvRegression-methods
#' @export
nobs.pvRegression <- function(object, ...) {
  sum(vapply(.pvRegFits(object), `[[`, numeric(1), "n"))
}

#' @rdname pvRegression-methods
#' @export
predict.pvRegression <- function(object, newdata, se.fit = FALSE, ...) {
  if (missing(newdata)) stop("`newdata` is required for predict.pvRegression()")
  fits <- .pvRegFits(object)
  by <- attr(object, "by")
  grp <- if (is.null(by)) rep(names(fits), nrow(newdata)) else {
    if (!by %in% names(newdata)) stop("`newdata` must contain the `by` column '", by, "'")
    as.character(newdata[[by]])
  }
  unknown <- setdiff(unique(grp), names(fits))
  if (length(unknown) > 0) stop("unknown group(s) in newdata: ", paste(unknown, collapse = ", "))
  fit <- se <- rep(NA_real_, nrow(newdata))
  for (g in unique(grp)) {
    idx <- which(grp == g)
    f <- fits[[g]]
    mf <- stats::model.frame(f$terms, newdata[idx, , drop = FALSE],
                             na.action = stats::na.pass, xlev = f$xlevels)
    X <- stats::model.matrix(f$terms, mf, contrasts.arg = f$contrasts)
    fit[idx] <- drop(X %*% f$coefficients)
    if (se.fit) se[idx] <- sqrt(rowSums((X %*% f$vcov) * X))
  }
  names(fit) <- rownames(newdata)
  if (se.fit) list(fit = fit, se.fit = stats::setNames(se, rownames(newdata))) else fit
}

#' @rdname pvRegression-methods
#' @export
print.pvRegression <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  fits <- attr(x, "fits")
  if (is.null(fits)) return(print(as.data.frame(unclass(x)), ...))
  cat("\nPlausible-value regression\n\nCall:\n",
      paste(deparse(attr(x, "call")), collapse = "\n"), "\n", sep = "")
  for (g in names(fits)) {
    if (!is.null(attr(x, "by"))) cat("\n", attr(x, "by"), " = ", g, sep = "")
    cat("\nCoefficients:\n")
    print.default(format(fits[[g]]$coefficients, digits = digits),
                  print.gap = 2L, quote = FALSE)
  }
  cat("\n")
  invisible(x)
}

#' @rdname pvRegression-methods
#' @export
summary.pvRegression <- function(object, ...) {
  fits <- .pvRegFits(object)
  df <- as.data.frame(unclass(object))
  tabs <- lapply(names(fits), function(g) {
    f <- fits[[g]]
    rows <- if (is.null(attr(object, "by"))) df else df[df[[attr(object, "by")]] == g, ]
    rows <- rows[match(names(f$coefficients), rows$term), ]
    tab <- cbind(Estimate = rows$estimate, `Std. Error` = rows$se,
                 `z value` = rows$t, `Pr(>|z|)` = 2 * stats::pnorm(-abs(rows$t)))
    rownames(tab) <- rows$term
    r2 <- df[df$term == "R-squared", , drop = FALSE]
    if (!is.null(attr(object, "by"))) r2 <- r2[r2[[attr(object, "by")]] == g, ]
    list(coefficients = tab, r.squared = r2$estimate, r.squared.se = r2$se, n = f$n)
  })
  names(tabs) <- names(fits)
  structure(list(call = attr(object, "call"), by = attr(object, "by"),
                 groups = tabs, nPVs = df$nPVs[1], nReps = df$nReps[1],
                 repMethod = attr(object, "repMethod")),
            class = "summary.pvRegression")
}

#' @rdname pvRegression-methods
#' @export
print.summary.pvRegression <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nPlausible-value regression\n\nCall:\n",
      paste(deparse(x$call), collapse = "\n"), "\n", sep = "")
  cat("\nPVs: ", x$nPVs, "   Replicates: ", x$nReps,
      if (!is.na(x$repMethod)) paste0(" (", x$repMethod, ")"),
      "\nSEs combine replicate and imputation variance (Rubin's rules);",
      " p-values use the normal approximation.\n", sep = "")
  for (g in names(x$groups)) {
    s <- x$groups[[g]]
    if (!is.null(x$by)) cat("\n", x$by, " = ", g, sep = "")
    cat("\nCoefficients:\n")
    stats::printCoefmat(s$coefficients, digits = digits, has.Pvalue = TRUE, ...)
    cat("R-squared: ", formatC(s$r.squared, digits = digits),
        " (SE ", formatC(s$r.squared.se, digits = digits), ")   n = ", s$n, "\n", sep = "")
  }
  invisible(x)
}

#' @title pvCorMatrix
#'
#' @description Weighted Pearson correlation matrix of several measures,
#'   with standard errors from replicate weights and PVs. Each measure may be
#'   latent (a set of M plausible values, e.g., the outcome domains in IELS,
#'   PISA or TIMSS) or manifest (a single observed column, e.g., SES or
#'   age), so matrices mixing latent and observed variables can be produced.
#'
#'   For each PV draw m and each weight (full-sample and replicates), the
#'   full correlation matrix is computed in C++ in one pass over the data.
#'   Each element is then combined across the M draws and replicates with
#'   Rubin's rules as in [pvStat()] (see its "Variance components" section).
#'
#' @section Latent and manifest measures:
#'   **Pairing.** Latent measures are paired by position: the correlation
#'   between latent A and B uses PV1 of A with PV1 of B, PV2 with PV2, and
#'   so on. This is appropriate when the PVs for all latent measures were
#'   drawn jointly from one multidimensional model (as in ConQuest), so the
#'   m-th PV of each comes from the same draw. If the latent measures were
#'   scaled in separate (e.g., unidimensional) runs, pairing by position has
#'   no special meaning, and the correlation will generally be biased towards
#'   zero. A manifest variable is the same in every draw, so its single
#'   column is reused for each of the M draws.
#'
#'   Consequently, for each pair of measures:
#'
#'   | pair | estimate | imputationVar | nPVs |
#'   |------|----------|---------------|------|
#'   | latent-latent | mean over M paired draws | varies | M |
#'   | latent-manifest | mean over M draws (standard PISA/TIMSS method) | varies | M |
#'   | manifest-manifest | ordinary weighted statistic | 0 | 1 |
#'
#'   If no latent measure is given, the result is the ordinary weighted
#'   correlation (or covariance) matrix with replicate standard errors.
#'
#'   **Conditioning.** PVs reproduce the correlation with a background
#'   variable without bias only if that variable (or a good proxy for it)
#'   was in the population (conditioning) model used to draw them. A
#'   correlation between PVs and a manifest variable that was *not* in the
#'   conditioning model is biased towards zero. This cannot be detected
#'   from the data; check the scaling documentation of the study.
#'
#'   **Single columns.** A measure with one column is always treated as
#'   manifest. A latent measure must therefore have at least two PVs, and
#'   all latent measures must have the same number of PVs.
#'
#'   **Validity of the matrix.** Every measure is evaluated on the same cases
#'   (listwise deletion), so each per-draw matrix is a valid (positive
#'   semi-definite) correlation matrix, and so is their average
#'   (`combine = "raw"`). With `combine = "fisherZ"`, elements are
#'   back-transformed separately and the matrix is no longer guaranteed to
#'   be positive semi-definite; a warning is given if it is not. Covariance
#'   matrices from [pvCovMatrix()] are always averaged on the raw scale.
#'
#'   **Degenerate variables.** A measure that is constant within a group
#'   (e.g., a dummy variable with only one value in that group, or the `by`
#'   variable itself) has no defined correlation; the affected elements are
#'   `NA` and a warning names them.
#'
#' @inheritParams pvCor
#' @param pvs A named list with one element per measure. Each element is a
#'   character vector: the PV columns in draw order for a latent measure, or
#'   a single column name for a manifest variable, e.g.
#'   `list(literacy = paste0("PV", 1:5, "LIT"), numeracy = paste0("PV", 1:5, "NUM"), ses = "ESCS")`.
#'   Unnamed manifest elements are named by their column; unnamed latent
#'   elements `dim1`, `dim2`, ...
#' @param by Optional name of a grouping column; correlations are computed
#'   within each level.
#' @param format Either `"long"` (a data frame with one row per pair of
#'   measures) or `"matrix"` (a list of D x D matrices).
#' @return With `format = "long"`, a data frame with columns `dim1`, `dim2`,
#'   `estimate`, `se`, `samplingVar`, `imputationVar`, `ciLower`, `ciUpper`,
#'   `combine`, `nPVs`, `nReps`, `n` (and `zEstimate`, `zSe` with
#'   `combine = "fisherZ"`), plus a leading `by` column when `by` is used.
#'   With `format = "matrix"`, a list with elements `estimate`, `se`,
#'   `ciLower`, `ciUpper` (D x D symmetric matrices, diagonal 1 for
#'   `estimate` and `ciLower`/`ciUpper`, 0 for `se`), `n` and `latent` (a
#'   named logical vector, `TRUE` for latent measures); when `by` is used, a
#'   named list of such lists, one per group.
#'
#'   Cases with a missing value on any measure, weight or `by` column are
#'   dropped (listwise), so all pairs use the same cases.
#' @examples
#' set.seed(2718)
#' n <- 300
#' df <- data.frame(w = runif(n, 1, 3), ses = rnorm(n))
#' g <- 0.5 * df$ses + rnorm(n)
#' for (d in c("LIT", "NUM", "SR")) for (i in 1:5) {
#'   df[[paste0("PV", i, d)]] <- 0.7 * g + rnorm(n, 0, 0.7)
#' }
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' pvList <- lapply(c(lit = "LIT", num = "NUM", sr = "SR"), function(d) paste0("PV", 1:5, d))
#' pvCorMatrix(df, c(pvList, ses = "ses"), weight = "w",
#'             repWeights = paste0("rw", 1:20), repMethod = "JK1", format = "matrix")
pvCorMatrix <- function(data, pvs, weight = NULL, repWeights = NULL,
                        repMethod = c("BRR", "Fay", "JK1", "JK2"), by = NULL,
                        fayK = 0.5, repScale = NULL,
                        combine = c("raw", "fisherZ"), confLevel = 0.95,
                        format = c("long", "matrix")) {
  .pvMatrixEngine(data, pvs, weight, repWeights, match.arg(repMethod), by,
                  fayK, repScale, match.arg(combine), confLevel,
                  match.arg(format), type = "cor")
}

#' @title pvCovMatrix
#'
#' @description Weighted covariance matrix of several measures, with
#'   standard errors from replicate weights and PVs. Each measure may be
#'   latent (a set of M plausible values; e.g., the variances and
#'   covariances of the outcome domains in IELS, PISA or TIMSS) or manifest
#'   (a single observed column).
#'
#'   As in [pvCorMatrix()], for each PV draw and each weight the full
#'   D x D covariance matrix is computed in C++; each element is then
#'   combined across draws and replicates with Rubin's rules (see the
#'   "Variance components" section of [pvStat()]). The diagonal holds the
#'   variance of each measure and matches [pvVar()] applied to that
#'   measure's columns. See the "Latent and manifest measures" section of
#'   [pvCorMatrix()] for how latent and manifest measures are paired, and
#'   for caveats about conditioning.
#'
#'   Covariances use the same weighted estimator as [pvVar()]:
#'   `sum(w (x - xbar)(y - ybar)) / sum(w) * n / (n - 1)`, where n is the
#'   number of cases with positive weight. Because every element is
#'   averaged on the raw scale over the same cases, the combined matrix is
#'   positive semi-definite.
#'
#'   Confidence limits are normal approximations. For variances near zero, or
#'   with few replicates, they can be poor (and the lower limit can be
#'   negative), because the sampling distribution of a variance is skewed.
#'
#' @inheritParams pvCorMatrix
#' @param pvs A named list with one element per measure: the PV columns in
#'   draw order for a latent measure, or a single column for a manifest
#'   variable. A single measure is allowed (giving its variance).
#' @param format Either `"long"` (a data frame with one row per element of
#'   the lower triangle, including the diagonal) or `"matrix"` (a list of
#'   D x D matrices).
#' @return With `format = "long"`, a data frame with columns `dim1`, `dim2`
#'   (equal for variances), `estimate`, `se`, `samplingVar`, `imputationVar`,
#'   `ciLower`, `ciUpper`, `nPVs`, `nReps`, `n`, plus a leading `by` column
#'   when `by` is used. With `format = "matrix"`, a list with elements
#'   `estimate`, `se`, `ciLower`, `ciUpper` (D x D symmetric matrices), `n`
#'   and `latent`; when `by` is used, a named list of such lists, one per
#'   group. `imputationVar` is B before Rubin's correction, so
#'   `se^2 = samplingVar + (1 + 1/nPVs) * imputationVar`.
#'
#'   Cases with a missing value on any measure, weight or `by` column are
#'   dropped (listwise).
#' @examples
#' set.seed(2718)
#' n <- 300
#' df <- data.frame(w = runif(n, 1, 3), ses = rnorm(n))
#' g <- 0.5 * df$ses + rnorm(n)
#' for (d in c("LIT", "NUM", "SR")) for (i in 1:5) {
#'   df[[paste0("PV", i, d)]] <- 0.7 * g + rnorm(n, 0, 0.7)
#' }
#' for (r in 1:20) df[[paste0("rw", r)]] <- df$w * ifelse(seq_len(n) %% 20 == r - 1, 0, 20 / 19)
#' pvList <- lapply(c(lit = "LIT", num = "NUM", sr = "SR"), function(d) paste0("PV", 1:5, d))
#' pvCovMatrix(df, c(pvList, ses = "ses"), weight = "w",
#'             repWeights = paste0("rw", 1:20), repMethod = "JK1", format = "matrix")
pvCovMatrix <- function(data, pvs, weight = NULL, repWeights = NULL,
                        repMethod = c("BRR", "Fay", "JK1", "JK2"), by = NULL,
                        fayK = 0.5, repScale = NULL, confLevel = 0.95,
                        format = c("long", "matrix")) {
  .pvMatrixEngine(data, pvs, weight, repWeights, match.arg(repMethod), by,
                  fayK, repScale, "raw", confLevel, match.arg(format),
                  type = "cov")
}

# Name unnamed measures: manifest by column name, latent by prefix + index
.pvNameMeasures <- function(x, prefix) {
  nm <- names(x)
  if (is.null(nm)) nm <- rep("", length(x))
  blank <- is.na(nm) | nm == ""
  single <- lengths(x) == 1
  nm[blank & single] <- vapply(x[blank & single], `[`, character(1), 1)
  nm[blank & !single] <- paste0(prefix, seq_along(x))[blank & !single]
  if (anyDuplicated(nm)) {
    stop("measure names must be unique; duplicated: ",
         paste(unique(nm[duplicated(nm)]), collapse = ", "))
  }
  names(x) <- nm
  x
}

# Shared engine for pvCor(), pvCorMatrix() and pvCovMatrix().
# `meas` is a list of character vectors: length 1 = manifest, length M = latent.
# Manifest columns are repeated M times so every measure has M "draws".
.pvMatrixEngine <- function(data, meas, weight, repWeights, repMethod, by,
                            fayK, repScale, combine, confLevel, format, type,
                            warnDropped = TRUE) {
  stopifnot(is.data.frame(data))
  minDims <- if (type == "cor") 2 else 1
  if (!is.list(meas) || length(meas) < minDims ||
      !all(vapply(meas, function(v) is.character(v) && length(v) >= 1, logical(1)))) {
    stop("`pvs` must be a list of at least ", if (minDims == 2) "two" else "one",
         " character vector", if (minDims == 2) "s", " (one per measure)")
  }
  meas <- .pvNameMeasures(meas, prefix = "dim")
  nm <- names(meas)
  latent <- lengths(meas) > 1
  M <- unique(lengths(meas)[latent])
  if (length(M) > 1) {
    stop("all latent measures must have the same number of PVs; got ",
         paste(paste0(nm[latent], " = ", lengths(meas)[latent]), collapse = ", "))
  }
  if (length(M) == 0) M <- 1L
  D <- length(meas)
  allCols <- unlist(meas, use.names = FALSE)
  if (anyDuplicated(allCols)) {
    stop("the same column is used more than once: ",
         paste(unique(allCols[duplicated(allCols)]), collapse = ", "))
  }
  cols <- c(allCols, weight, repWeights, by)
  missingCols <- setdiff(cols, names(data))
  if (length(missingCols) > 0) {
    stop("columns not found in data: ", paste(missingCols, collapse = ", "))
  }
  nonNum <- allCols[!vapply(data[allCols], is.numeric, logical(1))]
  if (length(nonNum) > 0) {
    stop("variables must be numeric: ", paste(nonNum, collapse = ", "))
  }
  keep <- stats::complete.cases(data[, unique(cols), drop = FALSE])
  if (any(!keep)) {
    if (warnDropped) warning(sum(!keep), " case(s) with missing values dropped")
    data <- data[keep, , drop = FALSE]
  }
  nReps <- length(repWeights)
  scale <- .pvRepScale(repMethod, nReps, fayK, repScale)
  zCrit <- stats::qnorm(1 - (1 - confLevel) / 2)

  # Measure-major column order (measure d, draw m at column d * M + m),
  # matching the C++ routines; manifest columns are repeated M times
  expanded <- unlist(lapply(meas, function(v) if (length(v) == 1) rep(v, M) else v),
                     use.names = FALSE)
  X <- vapply(expanded, function(cc) as.numeric(data[[cc]]), numeric(nrow(data)))
  if (!is.matrix(X)) X <- matrix(X, nrow = nrow(data))
  w0 <- if (is.null(weight)) rep(1, nrow(data)) else as.numeric(data[[weight]])
  W <- cbind(w0, as.matrix(data[, repWeights, drop = FALSE])); storage.mode(W) <- "double"
  groups <- if (is.null(by)) factor(rep("All", nrow(data))) else droplevels(as.factor(data[[by]]))

  # Lower-triangle pairs in the same (column-major) order as the C++ output
  tri <- if (type == "cor") lower.tri(diag(D)) else lower.tri(diag(D), diag = TRUE)
  lt <- which(tri, arr.ind = TRUE)
  lt <- lt[order(lt[, "col"], lt[, "row"]), , drop = FALSE]
  pairLatent <- latent[lt[, "col"]] | latent[lt[, "row"]]

  naPairs <- character(0)
  res <- lapply(levels(groups), function(g) {
    idx <- which(groups == g)
    Xg <- X[idx, , drop = FALSE]; Wg <- W[idx, , drop = FALSE]
    est <- if (type == "cor") pvRepCorMatCpp(Xg, Wg, D, M) else pvRepCovMatCpp(Xg, Wg, D, M)
    out <- do.call(rbind, lapply(seq_len(nrow(lt)), function(k) {
      e <- matrix(est[k, , ], nrow = M)
      # Manifest-manifest: all draws identical, so use one (B = 0, nPVs = 1)
      if (!pairLatent[k]) e <- e[1, , drop = FALSE]
      if (anyNA(e)) {
        naPairs <<- c(naPairs, paste0(nm[lt[k, "col"]], "-", nm[lt[k, "row"]],
                                      if (!is.null(by)) paste0(" (", g, ")")))
      }
      .pvCorCombine(e, scale, combine, zCrit)
    }))
    d <- data.frame(group = g, dim1 = nm[lt[, "col"]], dim2 = nm[lt[, "row"]], out,
                    row.names = NULL)
    if (type == "cor") d$combine <- combine
    d$nPVs <- ifelse(pairLatent, M, 1L); d$nReps <- nReps; d$n <- length(idx)
    d
  })
  if (length(naPairs) > 0) {
    warning("undefined (NA) ", if (type == "cor") "correlations" else "covariances",
            " for some PV draws or replicates, usually because a measure is constant ",
            "within a group or replicate: ", paste(naPairs, collapse = ", "))
  }
  long <- do.call(rbind, res)

  toMat <- function(d) {
    fill <- function(col, diagVal) {
      m <- diag(diagVal, D)
      dimnames(m) <- list(nm, nm)
      m[cbind(match(d$dim2, nm), match(d$dim1, nm))] <- d[[col]]
      m[cbind(match(d$dim1, nm), match(d$dim2, nm))] <- d[[col]]
      m
    }
    # For covariances the diagonal is overwritten by the variances in `d`
    list(estimate = fill("estimate", 1), se = fill("se", 0),
         ciLower = fill("ciLower", 1), ciUpper = fill("ciUpper", 1), n = d$n[1],
         latent = stats::setNames(latent, nm))
  }
  mats <- lapply(split(long, factor(long$group, levels = levels(groups))), toMat)

  # Raw averaging over common cases is PSD; Fisher-z back-transformation need not be
  if (type == "cor" && combine == "fisherZ" && D > 2) {
    notPsd <- names(mats)[vapply(mats, function(m) {
      e <- m$estimate
      !anyNA(e) && min(eigen(e, symmetric = TRUE, only.values = TRUE)$values) < -1e-10
    }, logical(1))]
    if (length(notPsd) > 0) {
      warning("the Fisher-z combined correlation matrix is not positive semi-definite",
              if (!is.null(by)) paste0(" for group(s): ", paste(notPsd, collapse = ", ")),
              "; consider combine = \"raw\"")
    }
  }

  if (format == "long") {
    if (is.null(by)) long$group <- NULL else names(long)[1] <- by
    return(long)
  }
  if (is.null(by)) mats[[1]] else mats
}
