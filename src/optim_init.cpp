// #define ARMA_NO_DEBUG // For the final version

// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

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
      arma::vec xyd = xy.col(i) - xy.col(j);
      double D = arma::as_scalar(xyd.t()*xyd) - std::pow(d(k), 2);
      if (disjoint(k) && (D >= 0)) {
        continue;
      } else if (contained(k) && (D < 0)) {
        continue;
      } else {
        loss[0] += std::pow(D, 2);
        gradMat.col(i) += 4*D*xyd;
        gradMat.col(j) -= 4*D*xyd;
      }
    }
  }

  loss.attr("gradient") = Rcpp::wrap(arma::vectorise(gradMat, 1));

  return loss;
}


// // Computes the loss, gradient, and hessian
// // Currently not working.
// // [[Rcpp::export]]
// Rcpp::NumericVector optim_init(const arma::vec& par,
//                                const arma::mat& d,
//                                const arma::umat& disjoint,
//                                const arma::umat& contained) {
//   arma::uword n = par.n_elem/2;
//   arma::vec x = par.head(n);
//   arma::vec y = par.tail(n);
//
//   Rcpp::NumericVector loss(1, 0.0);
//   Rcpp::NumericVector grad(2*n, 0.0);
//   arma::mat hess(2*n, 2*n, arma::fill::zeros);
//
//   for (arma::uword i = 0; i < n; ++i) {
//     for (arma::uword j = i + 1; j < n; ++j) {
//       double xd = x(i) - x(j);
//       double yd = y(i) - y(j);
//       double D = std::pow(xd, 2) + std::pow(yd, 2) - std::pow(d(i, j), 2);
//       if (disjoint(i, j) && (D >= 0)) {
//         continue;
//       } else if (contained(i, j) && (D < 0)) {
//         continue;
//       } else {
//         loss[0]     += std::pow(D, 2);
//         grad[i]     += 4*xd*D;
//         grad[j]     -= 4*xd*D;
//         grad[i + n] += 4*yd*D;
//         grad[j + n] -= 4*yd*D;
//       }
//     }
//   }
//
//   for (arma::uword i = 0; i < n; ++i) {
//     for (arma::uword j = i; j < n; ++j) {
//       double xd = x(i) - x(j);
//       double yd = y(i) - y(j);
//       double D = std::pow(xd, 2) + std::pow(yd, 2) - std::pow(d(i, j), 2);
//       if (disjoint(i, j) && (D >= 0)) {
//         continue;
//       } else if (contained(i, j) && (D < 0)) {
//         continue;
//       } else {
//         hess(i, j)         = -4*D - 8*std::pow(xd, 2);
//         hess(i + n, j)     = -8*xd*yd;
//         hess(i, j + n)     = -8*xd*yd;
//         hess(i + n, j + n) = -4*D - 8*std::pow(yd, 2);
//       }
//     }
//   }
//
//   hess.submat(0, 0, arma::size(n, n)) = arma::symmatu(hess.submat(0, 0, arma::size(n, n)));
//   hess.submat(0, n, arma::size(n, n)) = arma::symmatu(hess.submat(0, n, arma::size(n, n)));
//   hess.submat(n, 0, arma::size(n, n)) = arma::symmatu(hess.submat(n, 0, arma::size(n, n)));
//   hess.submat(n, n, arma::size(n, n)) = arma::symmatu(hess.submat(n, n, arma::size(n, n)));
//
//   loss.attr("gradient") = grad;
//   loss.attr("hessian") = hess;
//
//   return loss;
// }
// //4((x−a)2+(y−b)2−d2) + 8(x−a)2
