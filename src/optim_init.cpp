#include <Rcpp.h>

// [[Rcpp::export]]
double
optim_init_loss(const Rcpp::NumericVector& par,
                const Rcpp::NumericMatrix& d,
                const Rcpp::LogicalMatrix& disjoint,
                const Rcpp::LogicalMatrix& subset) {
  const unsigned int n = par.size()/2;
  const Rcpp::NumericVector x = Rcpp::head(par, n);
  const Rcpp::NumericVector y = Rcpp::tail(par, n);

  double loss = 0.0;
  for (unsigned int i = 0; i < (n - 1); ++i) {
    for (unsigned int j = i + 1; j < n; ++j) {
      double D = std::pow(x(j) - x(i), 2) + std::pow(y(j) - y(i), 2) -
        std::pow(d(j, i), 2);
      if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
        continue;
      } else {
        loss += D*D;
      }
    }
  }
  return loss;
}

// [[Rcpp::export]]
Rcpp::NumericVector
optim_init_grad(const Rcpp::NumericVector& par,
                const Rcpp::NumericMatrix& d,
                const Rcpp::LogicalMatrix& disjoint,
                const Rcpp::LogicalMatrix& subset) {
  const unsigned int n = par.size()/2;
  const Rcpp::NumericVector x = Rcpp::head(par, n);
  const Rcpp::NumericVector y = Rcpp::tail(par, n);

  Rcpp::NumericVector grad(2*n);
  for (unsigned int i = 0; i < (n - 1); ++i) {
    for (unsigned int j = i + 1; j < n; ++j) {
      double xd = x(j) - x(i);
      double yd = y(j) - y(i);
      double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
      if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
        continue;
      } else {
        grad[j]     += 4.0*xd*D;
        grad[i]     -= 4.0*xd*D;
        grad[j + n] += 4.0*yd*D;
        grad[i + n] -= 4.0*yd*D;
      }
    }
  }
  return grad;
}

// Compute the hessian (only the lower triangle)
// [[Rcpp::export]]
Rcpp::NumericMatrix
optim_init_hess(const Rcpp::NumericVector& par,
                const Rcpp::NumericMatrix& d,
                const Rcpp::LogicalMatrix& disjoint,
                const Rcpp::LogicalMatrix& subset) {
  const unsigned int n = par.size()/2;
  const Rcpp::NumericVector x = Rcpp::head(par, n);
  const Rcpp::NumericVector y = Rcpp::tail(par, n);

  Rcpp::NumericMatrix hess(2*n, 2*n);
  for (unsigned int i = 0; i < (n - 1); ++i) {
    for (unsigned int j = i + 1; j < n; ++j) {
      double xd = x(j) - x(i);
      double yd = y(j) - y(i);
      double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
      if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
        continue;
      } else {
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
  return hess;
}

// // Computes the loss, gradient, and hessian simultaneously. For use with nlm()
// // [[Rcpp::export]]
// Rcpp::NumericVector optim_init(const arma::vec& par,
//                                const arma::mat& d,
//                                const arma::umat& disjoint,
//                                const arma::umat& subset) {
//   arma::uword n = par.n_elem/2;
//   arma::vec x = par.head(n);
//   arma::vec y = par.tail(n);
//
//   Rcpp::NumericVector loss(1, 0.0);
//   Rcpp::NumericVector grad(2*n, 0.0);
//   //arma::mat hess(2*n, 2*n, arma::fill::zeros);
//
//   for (arma::uword i = 0; i < (n - 1); ++i) {
//     for (arma::uword j = i + 1; j < n; ++j) {
//       double xd = x(j) - x(i);
//       double yd = y(j) - y(i);
//       double D = xd*xd + yd*yd - std::pow(d(j, i), 2);
//       if ((disjoint(j, i) && (D >= 0.0)) || (subset(j, i) && (D < 0.0))) {
//         continue;
//       } else {
//         // Loss
//         loss[0] += D*D;
//
//         // Gradient
//         grad[j]     += 4.0*xd*D;
//         grad[i]     -= 4.0*xd*D;
//         grad[j + n] += 4.0*yd*D;
//         grad[i + n] -= 4.0*yd*D;
//
//         // // Hessian upper left
//         // hess(j, j) += 4.0*D + 8.0*xd*xd;
//         // hess(i, i) += 4.0*D + 8.0*xd*xd;
//         // hess(i, j) -= 4.0*D + 8.0*xd*xd;
//         //
//         // // Hessian lower right
//         // hess(j + n, j + n) += 4.0*D + 8.0*yd*yd;
//         // hess(i + n, i + n) += 4.0*D + 8.0*yd*yd;
//         // hess(i + n, j + n) -= 4.0*D + 8.0*yd*yd;
//         //
//         // // Hessian lower left
//         // hess(j + n, j) += 8.0*xd*yd;
//         // hess(i + n, i) += 8.0*xd*yd;
//         // hess(j + n, i) -= 8.0*xd*yd;
//         // hess(i + n, j) -= 8.0*xd*yd;
//       }
//     }
//   }
//
//   loss.attr("gradient") = grad;
//   //loss.attr("hessian") = arma::symmatl(hess);
//
//   return loss;
// }


