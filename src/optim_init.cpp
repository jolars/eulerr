// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

// #define ARMA_NO_DEBUG

// Loss function for the intial optimizer.
// [[Rcpp::export]]
double optim_init_loss(
    const arma::vec& par,
    const arma::vec& d,
    const arma::uvec& disjoint,
    const arma::uvec& contained
  ) {
  arma::uword n = par.n_elem/2;
  arma::mat xy = arma::reshape(par, n, 2).t();

  double out = 0;
  for (arma::uword i = 0, k = 0; i < n; i++) {
    for (arma::uword j = i + 1; j < n; j++, k++) {
      arma::vec xycold = xy.col(i) - xy.col(j);
      double D = arma::as_scalar(xycold.t() * xycold) - std::pow(d(k), 2);
      if (disjoint(k) && (D >= 0)) {
        continue;
      } else if (contained(k) && (D < 0)) {
        continue;
      } else {
        out += std::pow(D, 2);
      }
    }
  }
  return out;
}

// Gradient for the initial optimizer.
// [[Rcpp::export]]
SEXP optim_init_grad(
    const arma::rowvec& par,
    const arma::vec& d,
    const arma::uvec& disjoint,
    const arma::uvec& contained
  ) {
  arma::uword n = par.n_elem/2;
  arma::mat xy = arma::reshape(par, n, 2).t();

  arma::mat out(2, n, arma::fill::zeros);
  for (arma::uword i = 0, k = 0; i < n; i++) {
    for (arma::uword j = i + 1; j < n; j++, k++) {
      arma::vec xycold = xy.col(i) - xy.col(j);
      double D = arma::as_scalar(xycold.t() * xycold) - pow(d(k), 2);
      if (disjoint(k) && (D >= 0)) {
        continue;
      } else if (contained(k) && (D < 0)) {
        continue;
      } else {
        out.col(i) += (4 * (D)) * xycold;
        out.col(j) -= (4 * (D)) * xycold;
      }
    }
  }
  return Rcpp::wrap(arma::vectorise(out, 1));
}
