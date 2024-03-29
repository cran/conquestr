// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// replaceInVector
NumericVector replaceInVector(NumericVector v, double r, double x);
RcppExport SEXP _conquestr_replaceInVector(SEXP vSEXP, SEXP rSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(replaceInVector(v, r, x));
    return rcpp_result_gen;
END_RCPP
}
// replaceInDataFrame
DataFrame replaceInDataFrame(DataFrame d, double r, double x);
RcppExport SEXP _conquestr_replaceInDataFrame(SEXP dSEXP, SEXP rSEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type d(dSEXP);
    Rcpp::traits::input_parameter< double >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(replaceInDataFrame(d, r, x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_conquestr_replaceInVector", (DL_FUNC) &_conquestr_replaceInVector, 3},
    {"_conquestr_replaceInDataFrame", (DL_FUNC) &_conquestr_replaceInDataFrame, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_conquestr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
