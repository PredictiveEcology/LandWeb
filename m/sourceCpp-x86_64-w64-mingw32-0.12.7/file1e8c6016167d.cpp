#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector runifC(const int N) {
              NumericVector X(N);
              X = runif(N);
              return X;
              }
