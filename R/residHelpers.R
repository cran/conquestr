#' @title Correct Yen's Q3 for its expected value
#'
#' @description Shifts the off-diagonal elements of a matrix of residual
#'   correlations (Yen's \eqn{Q_3}) so that their expectation under local
#'   independence is approximately zero.
#'
#' @details
#' Yen's (1984) \eqn{Q_3} for items \eqn{i} and \eqn{j} is the Pearson
#' correlation, across persons \eqn{n = 1, \ldots, N}, of the raw item residuals
#' \deqn{d_{ni} = x_{ni} - E(X_{ni} \mid \hat\theta_n), \qquad Q_{3,ij} = r(d_i, d_j).}
#' Chou and Wang (2010) instead correlate the standardised residuals
#' \eqn{z_{ni} = d_{ni} / \sqrt{\mathrm{Var}(X_{ni} \mid \hat\theta_n)}}{z_ni = d_ni / sqrt(Var(X_ni | theta_n))}.
#' [steigerStat()] uses whichever residuals it is given.
#'
#' When abilities are maximum-likelihood (ML) or weighted-likelihood (WLE)
#' estimates from the same responses, the residuals are constrained, and under
#' local independence the expected value of \eqn{Q_3} is approximately
#' \eqn{-1/(L-1)} rather than 0, where \eqn{L} is the number of items
#' (Yen, 1993; Chou & Wang, 2010). For these abilities the function removes
#' that expectation from every off-diagonal element:
#' \deqn{Q_{3,ij}^{*} = Q_{3,ij} + \frac{1}{L-1}, \quad i \neq j.}
#' The diagonal is left at 1. Values pushed outside \eqn{[-1, 1]} are clamped
#' to the boundary.
#'
#' **The \eqn{-1/(L-1)} expectation assumes ML/WLE abilities.** EAP estimates
#' are shrunk towards the population mean, and plausible values (PVs) are random
#' draws from each person's posterior. Either way the residuals are less tightly
#' constrained, so the null mean of \eqn{Q_3} is closer to 0, and adding
#' \eqn{1/(L-1)} over-corrects and inflates apparent dependence. In a
#' simulated Rasch example (\eqn{N = 1000}, \eqn{L = 10}) the null mean was
#' \eqn{-0.111} with ML abilities but \eqn{-0.075} with EAP. For
#' `abilityType = "EAP"` or `"PV"` the correction is therefore turned off,
#' and the input is returned unchanged after validation. No analytic
#' expectation is applied for these cases.
#'
#' @param myCorMat A correlation matrix (unit diagonal, no missing values) of
#'   item residuals, with at least 2 items.
#' @param abilityType The type of ability estimate used to compute the
#'   residuals: one of `"ML"` (default), `"WLE"`, `"EAP"` or `"PV"`. The
#'   \eqn{-1/(L-1)} correction is applied only for `"ML"` and `"WLE"`.
#' @return A matrix of the same dimensions (and dimnames). For ML/WLE it holds
#'   the corrected off-diagonal values \eqn{Q_{3,ij}^{*}}; for EAP/PV it is
#'   `myCorMat` unchanged.
#' @references
#' Yen, W. M. (1984). Effects of local item dependence on the fit and equating
#'   performance of the three-parameter logistic model.
#'   *Applied Psychological Measurement, 8*(2), 125--145.
#'   \doi{10.1177/014662168400800201}
#'
#' Yen, W. M. (1993). Scaling performance assessments: Strategies for managing
#'   local item dependence. *Journal of Educational Measurement, 30*(3),
#'   187--213. \doi{10.1111/j.1745-3984.1993.tb00423.x}
#'
#' Chou, Y.-T., & Wang, W.-C. (2010). Checking dimensionality in item response
#'   models with principal component analysis on standardized residuals.
#'   *Educational and Psychological Measurement, 70*(5), 717--731.
#'   \doi{10.1177/0013164410379322}
#' @seealso [fisherTrnsfrm()], [steigerStat()]
q3ExpCorrect <- function(myCorMat, abilityType = c("ML", "WLE", "EAP", "PV")) {
  abilityType <- match.arg(abilityType)
  .checkCorMat(myCorMat)
  if (nrow(myCorMat) < 2) stop("the Q3 correction requires at least 2 items")
  # -1/(L-1) is the null expectation only for ML/WLE abilities
  if (abilityType %in% c("EAP", "PV")) return(myCorMat)
  myCorrection <- -1/(nrow(myCorMat)-1)
  myResult <- myCorMat
  myResult[lower.tri(myResult)] <- myResult[lower.tri(myResult)] - myCorrection
  myResult[upper.tri(myResult)] <- myResult[upper.tri(myResult)] - myCorrection
  myResult[myResult > 1] <- 1
  myResult[myResult < -1] <- -1
  return(myResult)
}

#' @title Fisher z-transform a correlation matrix
#'
#' @description Applies Fisher's (1915) variance-stabilising transformation to
#'   the off-diagonal elements of a correlation matrix.
#'
#' @details
#' Each off-diagonal correlation is mapped to
#' \deqn{z_{ij} = \operatorname{artanh}(r_{ij}) = \frac{1}{2}\ln\frac{1 + r_{ij}}{1 - r_{ij}}, \quad i \neq j.}{z_ij = atanh(r_ij) = 0.5 * log((1 + r_ij) / (1 - r_ij)).}
#' For bivariate-normal data with population correlation \eqn{\rho},
#' \eqn{z_{ij}} is approximately normal with mean \eqn{\operatorname{artanh}(\rho)}
#' and variance \eqn{1/(N-3)}, independent of \eqn{\rho}.
#'
#' Since \eqn{\operatorname{artanh}(\pm 1) = \pm\infty}, off-diagonal values with
#' \eqn{|r_{ij}| \ge 1 - 10^{-7}} are first truncated to \eqn{\pm(1 - 10^{-7})}
#' (giving \eqn{|z_{ij}| \approx 8.4}) and a warning is issued. The diagonal is
#' left at 1 and is not meaningful on the z scale.
#'
#' @param myCorMat A correlation matrix (unit diagonal, no missing values).
#' @return A matrix of the same dimensions with \eqn{z_{ij}} off the diagonal
#'   and 1 on the diagonal.
#' @references
#' Fisher, R. A. (1915). Frequency distribution of the values of the
#'   correlation coefficient in samples from an indefinitely large population.
#'   *Biometrika, 10*(4), 507--521. \doi{10.2307/2331838}
#' @seealso [q3ExpCorrect()], [steigerStat()]
fisherTrnsfrm <- function(myCorMat) {
  # takes in a correlation matrix, returns a matrix where correaltions are fisher tranformed (r_ij becomes z_ij)
  .checkCorMat(myCorMat)
  off <- row(myCorMat) != col(myCorMat)
  # atanh(+/-1) is infinite; pull boundary values in so they stay large but finite
  bound <- 1 - 1e-7
  atBound <- off & abs(myCorMat) >= bound
  if (any(atBound)) {
    warning(sum(atBound[lower.tri(atBound)]), " off-diagonal correlation(s) at +/-1 were truncated to +/-",
            bound, " before the Fisher transform", call. = FALSE)
  }
  r <- pmin(pmax(myCorMat[off], -bound), bound)
  myResult <- myCorMat
  myResult[off] <- atanh(r)
  return(myResult)
}

.checkCorMat <- function(myCorMat, tol = sqrt(.Machine$double.eps)) {
  d <- diag(myCorMat)
  if (anyNA(myCorMat)) {
    stop("you must provide a correlation matrix without missing values ",
         "(an item with zero variance gives NA correlations)", call. = FALSE)
  }
  if (!isTRUE(all(abs(d - 1) < tol))) stop("you must provide a correlation matrix", call. = FALSE)
  invisible(TRUE)
}

#' @title Steiger test of local independence of residuals
#'
#' @description Tests whether the matrix of item-residual correlations (Yen's
#'   \eqn{Q_3}) is consistent with an identity matrix, i.e. whether the
#'   standardised residuals are mutually uncorrelated (local independence).
#'
#' @details
#' The input is an \eqn{N \times L}{N x L} matrix of standardised residuals
#' \deqn{z_{ni} = \frac{x_{ni} - E(X_{ni})}{\sqrt{\mathrm{Var}(X_{ni})}},}
#' where for a dichotomous item \eqn{E(X_{ni}) = p_{ni}} and
#' \eqn{\mathrm{Var}(X_{ni}) = p_{ni}(1 - p_{ni})}. The steps are:
#'
#' 1. **Residual correlations.** Rows with any missing value are dropped
#'    (listwise deletion) and the Pearson correlation matrix
#'    \eqn{R = [Q_{3,ij}]} is computed.
#' 2. **Q3 correction** (`q3Adj = TRUE`). For ML/WLE abilities each
#'    off-diagonal element is shifted by its approximate null expectation,
#'    \eqn{Q_{3,ij} + 1/(L-1)}. The correction assumes ML/WLE abilities; for
#'    `abilityType = "EAP"` or `"PV"` it is turned off. See [q3ExpCorrect()].
#' 3. **Fisher transform** (`fisher = TRUE`). Each off-diagonal element is
#'    transformed, \eqn{z_{ij} = \operatorname{artanh}(r_{ij})}; see
#'    [fisherTrnsfrm()].
#' 4. **Test statistic.** Following Steiger (1980), under
#'    \eqn{H_0: P = I} the \eqn{z_{ij}} are approximately independent
#'    \eqn{N(0, 1/(N-3))}, so
#'    \deqn{\chi^2 = (N - 3) \sum_{i < j} z_{ij}^2}
#'    is referred to a \eqn{\chi^2} distribution with
#'    \deqn{df = \frac{L(L-1)}{2}.}
#' 5. **Optional df adjustment** (`dfAdj = TRUE`). The degrees of freedom are
#'    replaced by the empirical approximation
#'    \deqn{df^{*} = \frac{L(L-1)}{2} + \frac{N}{70} - \frac{L}{4} - 12\,t
#'      - \frac{N t}{40} + \frac{2 L t}{3},}
#'    where \eqn{t} is the test-person match (`tpm`). Chou and Wang (2010)
#'    found that, even after the \eqn{-1/(L-1)} correction, the Type I error
#'    rate of the Steiger statistic remained inflated in some conditions
#'    (e.g., large samples and short tests). They derived this adjustment by
#'    stepwise regression on simulated data as a simple correction that works
#'    reasonably well across a variety of conditions. The coefficients are
#'    therefore an empirical calibration, not an analytic result, and are best
#'    trusted within the range of conditions they simulated. The adjustment
#'    applies only to the Steiger statistic.
#'
#'    The calibration data were 32 null (Rasch-generating, Rasch-analysed)
#'    conditions with 100 replications each: dichotomous items only, test
#'    lengths \eqn{L \in \{10, 20, 40, 60\}}, sample sizes
#'    \eqn{N \in \{100, 200, 500, 1500\}}, and test-person match
#'    \eqn{t \in \{0, -1\}}, where \eqn{t} is mean item difficulty minus mean
#'    person ability (item difficulties drawn from \eqn{U(-2, 2)}, abilities
#'    normal with variance 1). The response variable was the observed
#'    \eqn{\chi^2} minus its nominal df, regressed on \eqn{N}, \eqn{L},
#'    \eqn{t} and their interactions. Even with the adjustment, Type I error
#'    stayed above the acceptable range in a few conditions, most notably
#'    \eqn{N = 1500}, \eqn{L = 10}, \eqn{t = -1} (Chou & Wang, 2010, Table 4).
#'    Results outside these ranges (e.g., \eqn{t > 0}, polytomous items, or
#'    \eqn{N > 1500}) are extrapolations. The calibration also used
#'    ML-type (Winsteps) abilities with the \eqn{-1/(L-1)} correction applied,
#'    so with EAP or PV residuals the adjustment is untested.
#'
#' If `fisher = FALSE` the statistic is computed from the untransformed
#' correlations, which is only a reasonable approximation when all
#' correlations are small.
#'
#' @param myDat A data frame or matrix of standardised residuals, persons in
#'   rows and items in columns.
#' @param q3Adj Logical; apply the Q3 expectation correction (step 2)?
#' @param abilityType The ability estimate used to compute the residuals:
#'   `"ML"` (default), `"WLE"`, `"EAP"` or `"PV"`. Passed to [q3ExpCorrect()];
#'   the \eqn{-1/(L-1)} correction is applied only for `"ML"` and `"WLE"`.
#'   Ignored when `q3Adj = FALSE`.
#' @param fisher Logical; apply the Fisher z-transform (step 3)?
#' @param dfAdj Logical; adjust the degrees of freedom for sample size, number
#'   of items and targeting using the Chou and Wang (2010) adjustment (step 5)?
#'   Recommended when \eqn{N} is large relative to \eqn{L}; calibrated only for
#'   dichotomous Rasch data with \eqn{10 \le L \le 60},
#'   \eqn{100 \le N \le 1500} and \eqn{t \in [-1, 0]}; a warning is issued outside this range. If `TRUE`, supply `tpm`.
#' @param tpm A double indicating the test-person match, where 0 indicates that
#'   mean item difficulty is equal to mean person ability, and -1 indicates that
#'   mean item difficulty is 1 logit below mean person ability. Defaults to 0.
#' @return A list with elements:
#'   * `Cor`: the (corrected and/or transformed) residual correlation matrix;
#'   * `steigerStat`: the \eqn{\chi^2} statistic;
#'   * `Chisq_p`: the upper-tail p-value;
#'   * `Chisq_crit`: the 95th percentile of the reference \eqn{\chi^2};
#'   * `N`, `L`, `DF`: number of persons, items and degrees of freedom.
#' @references
#' Steiger, J. H. (1980). Tests for comparing elements of a correlation
#'   matrix. *Psychological Bulletin, 87*(2), 245--251.
#'   \doi{10.1037/0033-2909.87.2.245}
#'
#' Fisher, R. A. (1915). Frequency distribution of the values of the
#'   correlation coefficient in samples from an indefinitely large population.
#'   *Biometrika, 10*(4), 507--521. \doi{10.2307/2331838}
#'
#' Yen, W. M. (1984). Effects of local item dependence on the fit and equating
#'   performance of the three-parameter logistic model.
#'   *Applied Psychological Measurement, 8*(2), 125--145.
#'   \doi{10.1177/014662168400800201}
#'
#' Chou, Y.-T., & Wang, W.-C. (2010). Checking dimensionality in item response
#'   models with principal component analysis on standardized residuals.
#'   *Educational and Psychological Measurement, 70*(5), 717--731.
#'   \doi{10.1177/0013164410379322}
#' @seealso [q3ExpCorrect()], [fisherTrnsfrm()]
#' @importFrom stats cor na.omit pchisq qchisq
steigerStat <- function(myDat, q3Adj = TRUE, fisher = TRUE, dfAdj = FALSE, tpm,
                        abilityType = c("ML", "WLE", "EAP", "PV")) {
  abilityType <- match.arg(abilityType)
  if (missing(tpm)) tpm <- 0
  myResultList <- list()
  if (!"matrix" %in% class(myDat)) {
    # should test if data frame, and then cast to numeric
    # shoudl try this and handle exceptions
    myDatT <- myDat
    for (i in seq_len(ncol(myDatT))) {
      myDatT[ , i] <- as.numeric(myDatT[ , i])
    }
    myDatT <- as.matrix(myDatT)
  } else {
    myDatT <- myDat
  }
  myN <- nrow(myDatT)
  myL <- ncol(myDatT)
  myDf <- myL*(myL-1)/2
  myCorMat <- cor(na.omit(myDatT))
  if (q3Adj) myCorMat <- q3ExpCorrect(myCorMat, abilityType = abilityType)
  if (fisher) myCorMat <- fisherTrnsfrm(myCorMat)
  if (dfAdj) {
    .checkDfAdjRange(myN, myL, tpm)
    myDf <- myDf + 1/70*myN - 1/4*myL - 12*tpm - 1/40*myN*tpm + 2/3*myL*tpm
  }
  myResultList[["Cor"]] <- myCorMat
  myResult <- (myN - 3) * sum(myCorMat[lower.tri(myCorMat)]^2)
  myResultList[["steigerStat"]] <- myResult
  myChiSq <- pchisq(myResult, myDf, lower.tail = FALSE)
  myResultList[["Chisq_p"]] <- myChiSq
  myResultList[["Chisq_crit"]] <- qchisq(0.95, myDf)
  myResultList[["N"]] <- myN
  myResultList[["L"]] <- myL
  myResultList[["DF"]] <- myDf
  return(myResultList)
}

# Calibration range of the Chou & Wang (2010) df adjustment (their Table 4)
.checkDfAdjRange <- function(n, l, tpm) {
  if (!is.numeric(tpm) || length(tpm) != 1 || !is.finite(tpm)) {
    stop("`tpm` must be a single finite number", call. = FALSE)
  }
  out <- c(
    if (n < 100 || n > 1500) paste0("N = ", n, " (calibrated 100-1500)"),
    if (l < 10 || l > 60) paste0("L = ", l, " (calibrated 10-60)"),
    if (tpm < -1 || tpm > 0) paste0("tpm = ", tpm, " (calibrated -1 to 0)")
  )
  if (length(out)) {
    warning("dfAdj is outside the range Chou & Wang (2010) calibrated it on: ",
            paste(out, collapse = "; "), ". The adjusted df is an extrapolation.",
            call. = FALSE)
  }
  invisible(length(out) == 0)
}
