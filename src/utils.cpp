// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
IntegerMatrix choose_two(IntegerVector x) {
  unsigned int n = x.size();
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

// [[Rcpp::export]]
LogicalMatrix find_surrounding_sets(NumericVector xs,
                                    NumericVector ys,
                                    NumericVector x,
                                    NumericVector y,
                                    NumericVector r) {
  int n = x.length();
  LogicalMatrix out(n, xs.length());

  for (int i = 0; i < n; i++) {
    out(i, _) = (pow(xs - x[i], 2) + pow(ys - y[i], 2) <= pow(r[i], 2));
  }
  return out;
}

// [[Rcpp::export]]
arma::uword max_colmins(arma::mat x) {
  uword n = x.n_cols;
  vec mins(n);
  for (uword i = 0; i < n; i++) {
    mins(i) = x.col(i).min();
  }
  return mins.index_max() + 1;
}
