#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

namespace {

// An item's category logits are linear in theta: logit_k = slope_k * theta + icpt_k.
//   conquest: D * (a_k * x_k * theta - sum_{j<=k} (d_j + t_j))
//   muraki:   D * a_k * (x_k * theta - sum_{j<=k} d_j + sum_{j<=k} t_j)
// Precomputing slope/icpt once per item avoids repeating the cumulative sums
// for every case.
struct ItemLin {
  std::vector<double> x;
  std::vector<double> slope;
  std::vector<double> icpt;
};

ItemLin makeItemLin(const NumericMatrix& item, bool muraki, double D) {
  int k = item.nrow();
  int nc = item.ncol();
  if (nc < 3) stop("item must be a matrix with at least scores, delta dots, and taus");
  ItemLin it;
  it.x.resize(k);
  it.slope.resize(k);
  it.icpt.resize(k);
  double cumD = 0.0, cumT = 0.0;
  for (int i = 0; i < k; ++i) {
    double a = (nc >= 4) ? item(i, 3) : 1.0;
    cumD += item(i, 1);
    cumT += item(i, 2);
    it.x[i] = item(i, 0);
    it.slope[i] = D * a * item(i, 0);
    it.icpt[i] = muraki ? D * a * (cumT - cumD) : -D * (cumD + cumT);
  }
  return it;
}

// category probabilities at theta, written into p (max logit subtracted for stability)
void catProbs(const ItemLin& it, double theta, std::vector<double>& p) {
  std::size_t k = it.x.size();
  p.resize(k);
  double mx = R_NegInf;
  for (std::size_t i = 0; i < k; ++i) {
    p[i] = it.slope[i] * theta + it.icpt[i];
    if (p[i] > mx) mx = p[i];
  }
  double denom = 0.0;
  for (std::size_t i = 0; i < k; ++i) {
    p[i] = std::exp(p[i] - mx);
    denom += p[i];
  }
  for (std::size_t i = 0; i < k; ++i) p[i] /= denom;
}

} // namespace

//' Response probabilities for each category of an item at theta (C++ backend of `simplep`)
//'
//' @param theta A double.
//' @param item A k by 4 (or 3) matrix: category score, delta dot, tau, discrimination.
//' @param muraki A Boolean. TRUE for Muraki's parameterisation, FALSE for ConQuest's.
//' @param D A double, the scaling constant.
//' @return A NumericVector of length k.
//' @keywords internal
// [[Rcpp::export]]
NumericVector simplepCpp(double theta, NumericMatrix item, bool muraki = false, double D = 1.0) {
  ItemLin it = makeItemLin(item, muraki, D);
  std::vector<double> p;
  catProbs(it, theta, p);
  return wrap(p);
}

//' Generate item responses for all cases and items (C++ backend of `genResponses`)
//'
//' Loops over cases (outer) and items (inner), drawing one uniform deviate per
//' non-missing case-item pair, in the same order as the original R implementation.
//'
//' @param abilities A cases by dimensions NumericMatrix.
//' @param itemParams A list of item matrices (see `simplep`).
//' @param itemDim An IntegerVector (1-based), the ability column for each item.
//' @param perturbItems A list of perturbed item matrices.
//' @param perturbIdx An IntegerVector of length `length(itemParams)`; 0 if the item
//'   is not perturbed, otherwise a 1-based index into `perturbItems`.
//' @param perturbCase A cases by `length(perturbItems)` LogicalMatrix; TRUE where
//'   the case receives the perturbed item.
//' @return A cases by items NumericMatrix of responses.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix genResponsesCpp(
    NumericMatrix abilities, List itemParams, IntegerVector itemDim,
    List perturbItems, IntegerVector perturbIdx, LogicalMatrix perturbCase
) {
  int nCases = abilities.nrow();
  int nDims = abilities.ncol();
  int nItems = itemParams.size();
  int nPert = perturbItems.size();

  if (itemDim.size() != nItems) stop("`itemDim` must be the same length as `itemParams`");
  if (perturbIdx.size() != nItems) stop("`perturbIdx` must be the same length as `itemParams`");
  if (nPert > 0 && (perturbCase.nrow() != nCases || perturbCase.ncol() != nPert)) {
    stop("`perturbCase` must be cases by length(`perturbItems`)");
  }

  std::vector<ItemLin> items(nItems);
  for (int j = 0; j < nItems; ++j) {
    if (itemDim[j] < 1 || itemDim[j] > nDims) stop("`itemDim` refers to a dimension not in `abilities`");
    if (perturbIdx[j] < 0 || perturbIdx[j] > nPert) stop("`perturbIdx` out of range");
    items[j] = makeItemLin(as<NumericMatrix>(itemParams[j]), false, 1.0);
  }
  std::vector<ItemLin> pItems(nPert);
  for (int j = 0; j < nPert; ++j) {
    pItems[j] = makeItemLin(as<NumericMatrix>(perturbItems[j]), false, 1.0);
  }

  NumericMatrix resp(nCases, nItems);
  std::vector<double> p;

  for (int c = 0; c < nCases; ++c) {
    for (int j = 0; j < nItems; ++j) {
      double theta = abilities(c, itemDim[j] - 1);
      if (ISNAN(theta)) {
        resp(c, j) = NA_REAL;
        continue;
      }
      const ItemLin* it = &items[j];
      int pj = perturbIdx[j];
      if (pj > 0 && perturbCase(c, pj - 1) == TRUE) it = &pItems[pj - 1];

      catProbs(*it, theta, p);
      double u = R::runif(0.0, 1.0);
      std::size_t k = p.size();
      std::size_t cat = k - 1; // guard against cdf rounding below 1
      double cdf = 0.0;
      for (std::size_t i = 0; i < k; ++i) {
        cdf += p[i];
        if (u < cdf) {
          cat = i;
          break;
        }
      }
      resp(c, j) = it->x[cat];
    }
  }
  return resp;
}
