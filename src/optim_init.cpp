// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Loss function for the intial optimizer.
// [[Rcpp::export]]
double optim_init_loss(
    arma::rowvec par,
    arma::vec d,
    arma::uvec disjoint,
    arma::uvec contained
  ) {
  uword n = par.n_elem/2;
  mat xy(2, n);
  xy.row(0) = par.head(n);
  xy.row(1) = par.tail(n);

  double out = 0;
  for (uword i = 0, k = 0; i < n; i++) {
    for (uword j = i + 1; j < n; j++, k++) {
      vec xycold = xy.col(i) - xy.col(j);
      double D = as_scalar(xycold.t() * xycold) - pow(d[k], 2);
      if (disjoint[k] && (D >= 0)) {
        continue;
      } else if (contained[k] && (D < 0)) {
        continue;
      } else {
        out += pow(D, 2);
      }
    }
  }
  return out;
}

// Gradient for the initial optimizer.
// [[Rcpp::export]]
std::vector<double> optim_init_grad(
    arma::rowvec par,
    arma::vec d,
    arma::uvec disjoint,
    arma::uvec contained
  ) {
  uword n = par.n_elem/2;
  mat xy(2, n);
  xy.row(0) = par.head(n);
  xy.row(1) = par.tail(n);

  mat out(2, n, fill::zeros);
  for (uword i = 0, k = 0; i < n; i++) {
    for (uword j = i + 1; j < n; j++, k++) {
      vec xycold = xy.col(i) - xy.col(j);
      double D = as_scalar(xycold.t() * xycold) - pow(d[k], 2);
      if (disjoint[k] && (D >= 0)) {
        continue;
      } else if (contained[k] && (D < 0)) {
        continue;
      } else {
        out.col(i) += (4 * (D)) * xycold;
        out.col(j) -= (4 * (D)) * xycold;
      }
    }
  }
  return conv_to< std::vector<double> >::from(vectorise(out, 1));
}
