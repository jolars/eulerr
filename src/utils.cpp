// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include "helpers.h"

// [[Rcpp::export]]
arma::umat choose_two(const arma::uvec& x) {
  arma::uword n = x.size();
  arma::umat m(n * (n - 1) / 2, 2);
  for (arma::uword i = 0, k = 0; i < n - 1; i++) {
    for (arma::uword j = i + 1; j < n; j++, k++) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }
  return m;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix bit_indexr(const arma::uword n) {
  return Rcpp::wrap(bit_index(n));
}

// [[Rcpp::export]]
Rcpp::NumericVector discdisc(const Rcpp::NumericVector& r1,
                             const Rcpp::NumericVector& r2,
                             const Rcpp::NumericVector& d) {
  Rcpp::NumericVector r1e = Rcpp::pow(r1, 2);
  Rcpp::NumericVector r2e = Rcpp::pow(r2, 2);
  Rcpp::NumericVector de  = Rcpp::pow(d, 2);

  return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
    r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}

// // [[Rcpp::export]]
// arma::uword max_colmins(const arma::mat& x) {
//   arma::uword n = x.n_cols;
//   arma::vec mins(n);
//   for (arma::uword i = 0; i < n; i++)
//     mins(i) = x.col(i).min();
//   return mins.index_max() + 1;
// }

// [[Rcpp::export]]
double venneuler_stress(const arma::vec& areas, const arma::vec& fit) {
  double sst   = arma::accu(arma::square(fit));
  double slope = arma::accu(areas % fit) / arma::accu(arma::square(areas));
  double sse   = arma::accu(arma::square(fit - areas * slope));
  return sse / sst;
}
