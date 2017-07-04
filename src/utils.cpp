// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "helpers.h"
using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
arma::umat choose_two(const arma::uvec x) {
  uword n = x.size();
  umat m(n * (n - 1) / 2, 2);
  for (uword i = 0, k = 0; i < n - 1; i++) {
    for (uword j = i + 1; j < n; j++, k++) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }
  return m;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix bit_indexr(const arma::uword n) {
  return wrap(bit_index(n));
}

// [[Rcpp::export]]
Rcpp::NumericVector discdisc(
    const Rcpp::NumericVector r1,
    const Rcpp::NumericVector r2,
    const Rcpp::NumericVector d
  ) {
  NumericVector r1e = pow(r1, 2);
  NumericVector r2e = pow(r2, 2);
  NumericVector de = pow(d, 2);

  return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
    r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix find_surrounding_sets(
    const arma::vec xs,
    const arma::vec ys,
    const arma::vec x,
    const arma::vec y,
    const arma::vec r
  ) {
  uword n1 = x.n_elem,
        n2 = xs.n_elem;
  umat out(n1, n2);

  for (uword i = 0; i < n1; i++)
    for (uword j = 0; j < n2; j++)
      out(i, j) = (pow(xs(j) - x(i), 2) + pow(ys(j) - y(i), 2) <= pow(r(i), 2));

  return wrap(out);
}

// [[Rcpp::export]]
arma::uword max_colmins(const arma::mat x) {
  uword n = x.n_cols;
  vec mins(n);
  for (uword i = 0; i < n; i++)
    mins(i) = x.col(i).min();
  return mins.index_max() + 1;
}



