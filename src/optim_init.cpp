#define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "helpers.h"

// Computes the loss, gradient, and hessian simultaneously. For use with nlm()
// [[Rcpp::export]]
Rcpp::NumericVector optim_init(const arma::vec& par,
                               const arma::mat& d,
                               const arma::umat& disjoint,
                               const arma::umat& subset) {
  arma::uword n = par.n_elem/2;
  arma::vec x = par.head(n);
  arma::vec y = par.tail(n);

  Rcpp::NumericVector loss(1, 0.0);
  Rcpp::NumericVector grad(2*n, 0.0);
  arma::mat hess(2*n, 2*n, arma::fill::zeros);

  for (arma::uword i = 0; i < (n - 1); ++i) {
    for (arma::uword j = i + 1; j < n; ++j) {
      double xd = x(j) - x(i);
      double yd = y(j) - y(i);
      double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
      if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
        continue;
      } else {
        // Loss
        loss[0] += D*D;

        // Gradient
        grad[j]     += 4.0*xd*D;
        grad[i]     -= 4.0*xd*D;
        grad[j + n] += 4.0*yd*D;
        grad[i + n] -= 4.0*yd*D;

        // Upper left
        hess(j, j) += 4.0*D + 8.0*xd*xd;
        hess(i, i) += 4.0*D + 8.0*xd*xd;
        hess(i, j) -= 4.0*D + 8.0*xd*xd;

        // Lower right
        hess(j + n, j + n) += 4.0*D + 8.0*yd*yd;
        hess(i + n, i + n) += 4.0*D + 8.0*yd*yd;
        hess(i + n, j + n) -= 4.0*D + 8.0*yd*yd;

        // Lower left
        hess(j + n, j) += 8.0*xd*yd;
        hess(i + n, i) += 8.0*xd*yd;
        hess(j + n, i) -= 8.0*xd*yd;
        hess(i + n, j) -= 8.0*xd*yd;
      }
    }
  }

  loss.attr("gradient") = grad;
  loss.attr("hessian") = arma::symmatl(hess);

  return loss;
}

// // [[Rcpp::export]]
// double optim_init_loss(const arma::vec& par,
//                        const arma::mat& d,
//                        const arma::umat& disjoint,
//                        const arma::umat& subset) {
//   arma::uword n = par.n_elem/2;
//   arma::vec x = par.head(n);
//   arma::vec y = par.tail(n);
//
//   double loss = 0.0;
//   for (arma::uword i = 0; i < (n - 1); ++i) {
//     for (arma::uword j = i + 1; j < n; ++j) {
//       double D = std::pow(x(j) - x(i), 2) + std::pow(y(j) - y(i), 2) -
//         std::pow(d(j, i), 2);
//       if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
//         continue;
//       } else {
//         loss += D*D;
//       }
//     }
//   }
//   return loss;
// }
//
// // [[Rcpp::export]]
// Rcpp::NumericVector optim_init_grad(const arma::vec& par,
//                                     const arma::mat& d,
//                                     const arma::umat& disjoint,
//                                     const arma::umat& subset) {
//   arma::uword n = par.n_elem/2;
//   arma::vec x = par.head(n);
//   arma::vec y = par.tail(n);
//
//   Rcpp::NumericVector grad(2*n);
//   for (arma::uword i = 0; i < (n - 1); ++i) {
//     for (arma::uword j = i + 1; j < n; ++j) {
//       double xd = x(j) - x(i);
//       double yd = y(j) - y(i);
//       double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
//       if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
//         continue;
//       } else {
//         grad[j]     += 4.0*xd*D;
//         grad[i]     -= 4.0*xd*D;
//         grad[j + n] += 4.0*yd*D;
//         grad[i + n] -= 4.0*yd*D;
//       }
//     }
//   }
//   return grad;
// }
//
// // Compute the hessian (only the lower triangle)
// // [[Rcpp::export]]
// arma::mat optim_init_hess(const arma::vec& par,
//                           const arma::mat& d,
//                           const arma::umat& disjoint,
//                           const arma::umat& subset) {
//   const arma::uword n = par.n_elem/2;
//   const arma::vec x = par.head(n);
//   const arma::vec y = par.tail(n);
//
//   arma::mat hess(2*n, 2*n, arma::fill::zeros);
//   for (arma::uword i = 0; i < (n - 1); ++i) {
//     for (arma::uword j = i + 1; j < n; ++j) {
//       double xd = x(j) - x(i);
//       double yd = y(j) - y(i);
//       double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
//       if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
//         continue;
//       } else {
//         // Upper left
//         hess(j, j) += 4.0*D + 8.0*xd*xd;
//         hess(i, i) += 4.0*D + 8.0*xd*xd;
//         hess(i, j) -= 4.0*D + 8.0*xd*xd;
//
//         // Lower right
//         hess(j + n, j + n) += 4.0*D + 8.0*yd*yd;
//         hess(i + n, i + n) += 4.0*D + 8.0*yd*yd;
//         hess(i + n, j + n) -= 4.0*D + 8.0*yd*yd;
//
//         // Lower left
//         hess(j + n, j) += 8.0*xd*yd;
//         hess(i + n, i) += 8.0*xd*yd;
//         hess(j + n, i) -= 8.0*xd*yd;
//         hess(i + n, j) -= 8.0*xd*yd;
//       }
//     }
//   }
//
//   return hess;
// }

// // Old Loss function for the initial optimizer.
// // [[Rcpp::export]]
// double optim_init_loss(const arma::rowvec& par,
//                        const arma::vec& d,
//                        const arma::uvec& disjoint,
//                        const arma::uvec& contained) {
//   arma::uword n = par.n_elem/2;
//   arma::mat xy = arma::reshape(par, n, 2).t();
//
//   double loss = 0.0;
//   for (arma::uword i = 0, k = 0; i < n; ++i) {
//     for (arma::uword j = i + 1; j < n; ++j, ++k) {
//       arma::vec xyd = xy.col(i) - xy.col(j);
//       double D = arma::as_scalar(xyd.t()*xyd) - std::pow(d(k), 2);
//       if ((disjoint(k) && (D >= 0.0)) || (contained(k) && (D < 0.0))) {
//       } else {
//         loss += D*D;
//       }
//     }
//   }
//   return loss;
// }
//
// // Old Gradient for the initial optimizer
// // [[Rcpp::export]]
// Rcpp::NumericVector optim_init_grad(const arma::rowvec& par,
//                                     const arma::vec& d,
//                                     const arma::uvec& disjoint,
//                                     const arma::uvec& contained) {
//   arma::uword n = par.n_elem/2;
//   arma::mat xy = arma::reshape(par, n, 2).t();
//
//   arma::mat grad_mat(2, n, arma::fill::zeros);
//   for (arma::uword i = 0, k = 0; i < n; ++i) {
//     for (arma::uword j = i + 1; j < n; ++j, ++k) {
//       arma::vec xyd = xy.col(i) - xy.col(j);
//       double D = arma::as_scalar(xyd.t()*xyd) - std::pow(d(k), 2);
//       if ((disjoint(k) && (D >= 0)) || (contained(k) && (D < 0))) {
//         continue;
//       } else {
//         grad_mat.col(i) += 4*D*xyd;
//         grad_mat.col(j) -= 4*D*xyd;
//       }
//     }
//   }
//   arma::vec out = arma::vectorise(grad_mat, 1);
//
//   return arma_to_rcpp(out);
// }
