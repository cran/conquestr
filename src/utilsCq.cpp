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


//' replace a very large negative number with something - usually NA_REAL
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

//' find the largest common substring in 2 strings
//'
//' @param str1 A string.
//' @param str2 A string.
// [[Rcpp::export]]
String lcs(String str1, String str2) { 
  std::string S = str1;
  std::string T = str2;
  int r = S.length();
  int n = T.length();
  std::vector<std::vector<int> > L(r , std::vector<int>(n));
  int z = 0;
  std::string ret;

  for (int i = 0; i < r; ++i)
  {
    for (int j = 0; j < n; ++j)
    {
      if (S[i] == T[j])
      {
        if (i == 0 || j == 0)
          L[i][j] = 1;
        else
          L[i][j] = L[i - 1][j - 1] + 1;

        if (L[i][j] > z)
        {
          z = L[i][j];
          ret = S.substr(i - z + 1, z);
        }
      }
      else
      {
        L[i][j] = 0;
      }
    }
  }
  return(ret);
}

//' find the largest common substring in a vector of strings
//'
//' @param input_strings A vector of strings.
// [[Rcpp::export]]
String lcs_vec(StringVector input_strings) { 
  int n = input_strings.size();
  std::string ret = "";
  
  if (n < 1) return(ret);
  if (n < 2) return(input_strings[0]);
   
  ret = lcs(input_strings[1], input_strings[2]);

  if (n < 3) return(ret);

  for (int i=0; i < n; i++) {  
    if (i < 2) continue;
    ret = lcs(ret, input_strings[i]);
  }
  return(ret);
}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
