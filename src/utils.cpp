// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
IntegerMatrix choose_two(IntegerVector x) {
  int n = x.size();
  IntegerMatrix m(n * (n - 1) / 2, 2);

  for (int i = 0, k = 0; i < n - 1; i++) {
    for (int j = i + 1; j < n; j++, k++) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }

  return m;
}

// [[Rcpp::export]]
NumericVector discdisc(NumericVector r1, NumericVector r2, NumericVector d) {
  NumericVector r1e = pow(r1, 2);
  NumericVector r2e = pow(r2, 2);
  NumericVector de = pow(d, 2);

  return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
    r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}
