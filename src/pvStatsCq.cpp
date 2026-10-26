#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <cmath>
using namespace Rcpp;

// Statistic codes (must match .pvStatCodes in R/pvStats.R)
// 1 = mean, 2 = variance, 3 = standard deviation, 4 = quantile, 5 = sum of weights,
// 6 = interquartile range (weighted q75 - q25)

static double wMean(const NumericMatrix::ConstColumn& x, const NumericMatrix::ConstColumn& w) {
  double sw = 0.0, swx = 0.0;
  int n = x.size();
  for (int i = 0; i < n; ++i) {
    if (w[i] <= 0.0) continue;
    sw += w[i];
    swx += w[i] * x[i];
  }
  return sw > 0.0 ? swx / sw : NA_REAL;
}

// Weighted variance with n/(n-1) correction, where n is the number of cases
// with positive weight. This is invariant to weight scaling.
static double wVar(const NumericMatrix::ConstColumn& x, const NumericMatrix::ConstColumn& w) {
  double sw = 0.0, swx = 0.0;
  int n = x.size(), nPos = 0;
  for (int i = 0; i < n; ++i) {
    if (w[i] <= 0.0) continue;
    sw += w[i];
    swx += w[i] * x[i];
    ++nPos;
  }
  if (nPos < 2 || sw <= 0.0) return NA_REAL;
  double m = swx / sw, ss = 0.0;
  for (int i = 0; i < n; ++i) {
    if (w[i] <= 0.0) continue;
    double d = x[i] - m;
    ss += w[i] * d * d;
  }
  return (ss / sw) * (double)nPos / (double)(nPos - 1);
}

// Weighted quantile on pre-sorted order `ord`. Returns the smallest x such that
// cumulative weight >= p * W; when cumulative weight equals p * W exactly, the
// average of that value and the next positive-weight value is returned.
static double wQuantile(const NumericMatrix::ConstColumn& x, const NumericMatrix::ConstColumn& w,
                        const std::vector<int>& ord, double p) {
  double sw = 0.0;
  int n = x.size();
  for (int i = 0; i < n; ++i) if (w[i] > 0.0) sw += w[i];
  if (sw <= 0.0) return NA_REAL;
  double target = p * sw, cum = 0.0;
  const double eps = 1e-12 * sw;
  for (int k = 0; k < n; ++k) {
    int i = ord[k];
    if (w[i] <= 0.0) continue;
    cum += w[i];
    if (cum >= target - eps) {
      if (std::fabs(cum - target) <= eps) {
        for (int k2 = k + 1; k2 < n; ++k2) {
          int j = ord[k2];
          if (w[j] > 0.0) return 0.5 * (x[i] + x[j]);
        }
      }
      return x[i];
    }
  }
  return x[ord[n - 1]];
}

//' Compute a weighted statistic over plausible values and replicate weights
//'
//' Internal workhorse for [pvStat()]. For each plausible value (column of `pv`)
//' and each weight vector (the full-sample weight followed by each replicate
//' weight), computes the requested statistic.
//'
//' @param pv A numeric matrix, n cases by M plausible values.
//' @param wts A numeric matrix, n cases by (R + 1) weights. Column 1 is the
//'   full-sample weight, columns 2..(R + 1) are the replicate weights.
//' @param stat An integer statistic code: 1 mean, 2 variance, 3 SD,
//'   4 quantile, 5 sum of weights, 6 IQR.
//' @param prob A double in \[0, 1\], used when `stat == 4`.
//' @return A numeric matrix, M by (R + 1), of estimates.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix pvRepStatCpp(const NumericMatrix& pv, const NumericMatrix& wts,
                           int stat, double prob) {
  int n = pv.nrow(), M = pv.ncol(), nW = wts.ncol();
  if (wts.nrow() != n) stop("pv and wts must have the same number of rows");
  if (stat < 1 || stat > 6) stop("unknown statistic code");
  if (stat == 4 && (prob < 0.0 || prob > 1.0)) stop("prob must be in [0, 1]");

  NumericMatrix out(M, nW);
  std::vector<int> ord(n);

  for (int m = 0; m < M; ++m) {
    NumericMatrix::ConstColumn x = pv(_, m);
    if (stat == 4 || stat == 6) {
      std::iota(ord.begin(), ord.end(), 0);
      std::sort(ord.begin(), ord.end(), [&x](int a, int b) { return x[a] < x[b]; });
    }
    for (int r = 0; r < nW; ++r) {
      NumericMatrix::ConstColumn w = wts(_, r);
      double v;
      switch (stat) {
        case 1: v = wMean(x, w); break;
        case 2: v = wVar(x, w); break;
        case 3: { double s2 = wVar(x, w); v = ISNAN(s2) ? NA_REAL : std::sqrt(s2); break; }
        case 4: v = wQuantile(x, w, ord, prob); break;
        case 6: v = wQuantile(x, w, ord, 0.75) - wQuantile(x, w, ord, 0.25); break;
        default: {
          double sw = 0.0;
          for (int i = 0; i < n; ++i) if (w[i] > 0.0) sw += w[i];
          v = sw;
        }
      }
      out(m, r) = v;
      if ((r & 63) == 0) checkUserInterrupt();
    }
  }
  return out;
}

// In-place Cholesky decomposition of a p x p SPD matrix (row-major vector).
// Returns false if the matrix is not positive definite.
static bool cholDecomp(std::vector<double>& A, int p) {
  for (int j = 0; j < p; ++j) {
    double s = A[j * p + j];
    for (int k = 0; k < j; ++k) s -= A[j * p + k] * A[j * p + k];
    if (s <= 1e-12 * std::max(1.0, std::fabs(A[j * p + j]))) return false;
    double d = std::sqrt(s);
    A[j * p + j] = d;
    for (int i = j + 1; i < p; ++i) {
      double t = A[i * p + j];
      for (int k = 0; k < j; ++k) t -= A[i * p + k] * A[j * p + k];
      A[i * p + j] = t / d;
    }
  }
  return true;
}

static void cholSolve(const std::vector<double>& L, int p, std::vector<double>& b) {
  for (int i = 0; i < p; ++i) {
    double t = b[i];
    for (int k = 0; k < i; ++k) t -= L[i * p + k] * b[k];
    b[i] = t / L[i * p + i];
  }
  for (int i = p - 1; i >= 0; --i) {
    double t = b[i];
    for (int k = i + 1; k < p; ++k) t -= L[k * p + i] * b[k];
    b[i] = t / L[i * p + i];
  }
}

//' Weighted least squares over PVs and replicate weights
//'
//' Internal workhorse for [pvRegression()]. The normal equations are formed
//' and Cholesky-factored once per weight and reused for every PV.
//'
//' @param X A numeric design matrix, n by p (including intercept if wanted).
//' @param pv A numeric matrix, n cases by M plausible values (outcomes).
//' @param wts A numeric matrix, n cases by (R + 1) weights (full-sample first).
//' @return A numeric array, (p + 1) by M by (R + 1): rows 1..p are the
//'   coefficients, row p + 1 is the weighted R-squared.
//' @keywords internal
// [[Rcpp::export]]
NumericVector pvRepRegCpp(const NumericMatrix& X, const NumericMatrix& pv,
                          const NumericMatrix& wts) {
  int n = X.nrow(), p = X.ncol(), M = pv.ncol(), nW = wts.ncol();
  if (pv.nrow() != n || wts.nrow() != n) stop("X, pv and wts must have the same number of rows");
  NumericVector out((p + 1) * M * nW);
  std::vector<double> A(p * p), b(p);
  for (int r = 0; r < nW; ++r) {
    NumericMatrix::ConstColumn w = wts(_, r);
    std::fill(A.begin(), A.end(), 0.0);
    double sw = 0.0;
    for (int i = 0; i < n; ++i) {
      double wi = w[i];
      if (wi <= 0.0) continue;
      sw += wi;
      for (int j = 0; j < p; ++j) {
        double xij = wi * X(i, j);
        for (int k = 0; k <= j; ++k) A[j * p + k] += xij * X(i, k);
      }
    }
    for (int j = 0; j < p; ++j) for (int k = j + 1; k < p; ++k) A[j * p + k] = A[k * p + j];
    bool ok = sw > 0.0 && cholDecomp(A, p);
    for (int m = 0; m < M; ++m) {
      int base = (p + 1) * (m + M * r);
      if (!ok) {
        for (int j = 0; j <= p; ++j) out[base + j] = NA_REAL;
        continue;
      }
      NumericMatrix::ConstColumn y = pv(_, m);
      std::fill(b.begin(), b.end(), 0.0);
      double sy = 0.0;
      for (int i = 0; i < n; ++i) {
        double wi = w[i];
        if (wi <= 0.0) continue;
        sy += wi * y[i];
        for (int j = 0; j < p; ++j) b[j] += wi * X(i, j) * y[i];
      }
      cholSolve(A, p, b);
      double my = sy / sw, sst = 0.0, sse = 0.0;
      for (int i = 0; i < n; ++i) {
        double wi = w[i];
        if (wi <= 0.0) continue;
        double fit = 0.0;
        for (int j = 0; j < p; ++j) fit += X(i, j) * b[j];
        sse += wi * (y[i] - fit) * (y[i] - fit);
        sst += wi * (y[i] - my) * (y[i] - my);
      }
      for (int j = 0; j < p; ++j) out[base + j] = b[j];
      out[base + p] = sst > 0.0 ? 1.0 - sse / sst : NA_REAL;
    }
    checkUserInterrupt();
  }
  out.attr("dim") = IntegerVector::create(p + 1, M, nW);
  return out;
}

//' Weighted correlation matrices across PV dimensions, per PV and weight
//'
//' Internal workhorse for [pvCorMatrix()]. PVs are paired by index across
//' dimensions: the m-th PV of each dimension is used together.
//'
//' @param pv A numeric matrix, n cases by (D * M) columns, ordered by
//'   dimension then PV: column `d * M + m` (0-based) is PV m of dimension d.
//' @param wts A numeric matrix, n cases by (R + 1) weights (full-sample first).
//' @param D Number of dimensions.
//' @param M Number of PVs per dimension.
//' @return A numeric array, P by M by (R + 1), where P = D (D - 1) / 2 and
//'   rows are the lower-triangle pairs in column-major order
//'   ((2,1), (3,1), ..., (D,1), (3,2), ...).
//' @keywords internal
// [[Rcpp::export]]
NumericVector pvRepCorMatCpp(const NumericMatrix& pv, const NumericMatrix& wts,
                             int D, int M) {
  int n = pv.nrow(), nW = wts.ncol();
  if (D < 2) stop("need at least two dimensions");
  if (pv.ncol() != D * M) stop("ncol(pv) must equal D * M");
  if (wts.nrow() != n) stop("pv and wts must have the same number of rows");
  int P = D * (D - 1) / 2;
  NumericVector out(P * M * nW);
  std::vector<double> mu(D), cp(D * D), dev(D);
  for (int r = 0; r < nW; ++r) {
    NumericMatrix::ConstColumn w = wts(_, r);
    double sw = 0.0;
    for (int i = 0; i < n; ++i) if (w[i] > 0.0) sw += w[i];
    for (int m = 0; m < M; ++m) {
      int base = P * (m + M * r);
      if (sw <= 0.0) {
        for (int k = 0; k < P; ++k) out[base + k] = NA_REAL;
        continue;
      }
      std::fill(mu.begin(), mu.end(), 0.0);
      std::fill(cp.begin(), cp.end(), 0.0);
      for (int i = 0; i < n; ++i) {
        if (w[i] <= 0.0) continue;
        for (int d = 0; d < D; ++d) mu[d] += w[i] * pv(i, d * M + m);
      }
      for (int d = 0; d < D; ++d) mu[d] /= sw;
      for (int i = 0; i < n; ++i) {
        double wi = w[i];
        if (wi <= 0.0) continue;
        for (int d = 0; d < D; ++d) dev[d] = pv(i, d * M + m) - mu[d];
        for (int a = 0; a < D; ++a) {
          double wa = wi * dev[a];
          for (int b = 0; b <= a; ++b) cp[a * D + b] += wa * dev[b];
        }
      }
      int k = 0;
      for (int b = 0; b < D; ++b) {
        for (int a = b + 1; a < D; ++a) {
          double den = cp[a * D + a] * cp[b * D + b];
          out[base + k++] = den > 0.0 ? cp[a * D + b] / std::sqrt(den) : NA_REAL;
        }
      }
    }
    checkUserInterrupt();
  }
  out.attr("dim") = IntegerVector::create(P, M, nW);
  return out;
}

//' Weighted covariance matrices across PV dimensions, per PV and weight
//'
//' Internal workhorse for [pvCovMatrix()]. PVs are paired by index across
//' dimensions, as in [pvRepCorMatCpp()]. Covariances use the same
//' n / (n - 1) correction as the weighted variance in [pvRepStatCpp()], where
//' n is the number of cases with positive weight.
//'
//' @inheritParams pvRepCorMatCpp
//' @return A numeric array, P by M by (R + 1), where P = D (D + 1) / 2 and
//'   rows are the lower triangle *including the diagonal* in column-major
//'   order ((1,1), (2,1), ..., (D,1), (2,2), (3,2), ...).
//' @keywords internal
// [[Rcpp::export]]
NumericVector pvRepCovMatCpp(const NumericMatrix& pv, const NumericMatrix& wts,
                             int D, int M) {
  int n = pv.nrow(), nW = wts.ncol();
  if (D < 1) stop("need at least one dimension");
  if (pv.ncol() != D * M) stop("ncol(pv) must equal D * M");
  if (wts.nrow() != n) stop("pv and wts must have the same number of rows");
  int P = D * (D + 1) / 2;
  NumericVector out(P * M * nW);
  std::vector<double> mu(D), cp(D * D), dev(D);
  for (int r = 0; r < nW; ++r) {
    NumericMatrix::ConstColumn w = wts(_, r);
    double sw = 0.0;
    int nPos = 0;
    for (int i = 0; i < n; ++i) if (w[i] > 0.0) { sw += w[i]; ++nPos; }
    for (int m = 0; m < M; ++m) {
      int base = P * (m + M * r);
      if (nPos < 2 || sw <= 0.0) {
        for (int k = 0; k < P; ++k) out[base + k] = NA_REAL;
        continue;
      }
      std::fill(mu.begin(), mu.end(), 0.0);
      std::fill(cp.begin(), cp.end(), 0.0);
      for (int i = 0; i < n; ++i) {
        if (w[i] <= 0.0) continue;
        for (int d = 0; d < D; ++d) mu[d] += w[i] * pv(i, d * M + m);
      }
      for (int d = 0; d < D; ++d) mu[d] /= sw;
      for (int i = 0; i < n; ++i) {
        double wi = w[i];
        if (wi <= 0.0) continue;
        for (int d = 0; d < D; ++d) dev[d] = pv(i, d * M + m) - mu[d];
        for (int a = 0; a < D; ++a) {
          double wa = wi * dev[a];
          for (int b = 0; b <= a; ++b) cp[a * D + b] += wa * dev[b];
        }
      }
      double f = (double)nPos / ((double)(nPos - 1) * sw);
      int k = 0;
      for (int b = 0; b < D; ++b) {
        for (int a = b; a < D; ++a) out[base + k++] = cp[a * D + b] * f;
      }
    }
    checkUserInterrupt();
  }
  out.attr("dim") = IntegerVector::create(P, M, nW);
  return out;
}
