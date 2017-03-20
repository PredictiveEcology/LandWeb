#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector runifC(const int N) {
              NumericVector X(N);
              X = runif(N);
              return X;
              }


#include <Rcpp.h>
// runifC
NumericVector runifC(const int N);
RcppExport SEXP sourceCpp_1_runifC(SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(runifC(N));
    return rcpp_result_gen;
END_RCPP
}
