#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


//' replace a very large neagtive number with something - usually NA_REAL
//'
//' @param v A NumericVector.
//' @param r A double - the value to be replaced if it is < -1e300.
//' @param x A double - the value to repalce r with.
// [[Rcpp::export]]
NumericVector replaceInVector(NumericVector v, double r, double x) {
  int n = v.length();
  NumericVector v1 = clone(v); //deep copy v so that this function is not destructive (shallow copy will result in v being edited by the code that follows)

  for (int i = 0; i < n; ++i) {
    if (v1[i] < -1e300 ) {
      v1[i] = x;
      //Rcout << "The value of r : " << r << " was changed to x : " << x << " \n"; //debug
    }
  }
  return v1;
}


//' iterate through a data frame and use replaceInVector
//'
//' @param d A DataFrame.
//' @param r A double - the value to be replaced if it is < -1e300.
//' @param x A double - the value to replace r with.
// [[Rcpp::export]]
DataFrame replaceInDataFrame(DataFrame d, double r, double x) {
  int n1 = d.length(); // number of cols in df
  DataFrame d1 = clone(d); //deep copy d so that this function is not destructive (shallow copy will result in d being edited by the code that follows)

  //for (DataFrame::iterator i = d1.begin(); i != d1.end(); ++i) {
  for (int i = 0; i < n1; ++i) {
    switch( TYPEOF(d1[i]) ) {
      case REALSXP: {
        NumericVector tmp = replaceInVector(d1[i], r, x);
        d1[i] = tmp;
        break;
      }
      default: {
        d1[i] = d1[i];
      }
    }
  }
  return(d1);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
