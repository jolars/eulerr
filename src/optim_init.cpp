// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

// #define ARMA_NO_DEBUG

// Loss and gradient for the initial optimizer.
// [[Rcpp::export]]
Rcpp::NumericVector optim_init(const arma::rowvec& par,
                               const arma::vec& d,
                               const arma::uvec& disjoint,
                               const arma::uvec& contained) {
  arma::uword n = par.n_elem/2;
  arma::mat xy = arma::reshape(par, n, 2).t();

  Rcpp::NumericVector loss(1, 0.0);
  arma::mat gradMat(2, n, arma::fill::zeros);
  for (arma::uword i = 0, k = 0; i < n; i++) {
    for (arma::uword j = i + 1; j < n; j++, k++) {
      arma::vec xycold = xy.col(i) - xy.col(j);
      double D = arma::as_scalar(xycold.t()*xycold) - std::pow(d(k), 2);
      if (disjoint(k) && (D >= 0)) {
        continue;
      } else if (contained(k) && (D < 0)) {
        continue;
      } else {
        loss[0] += std::pow(D, 2);
        gradMat.col(i) += (4*(D))*xycold;
        gradMat.col(j) -= (4*(D))*xycold;
      }
    }
  }

  loss.attr("gradient") = Rcpp::wrap(arma::vectorise(gradMat, 1));

  return loss;
}
