// Copyright Emanuel Huber
// This code has been ported to C++ from RConics version 1.0, which is licensed
// under the GPL-3 license.

#ifndef eulerr_solver_h_
#define eulerr_solver_h_

#include <RcppArmadillo.h>
#include "constants.h"
#include "helpers.h"

// Solve a cubic polynomial
arma::cx_vec solve_cubic(double alpha,
                         double beta,
                         double gamma,
                         double delta) {
  std::complex<double> i(0.0, 1.0);
  arma::cx_vec::fixed<3> y;

  double a = beta/alpha;
  double b = gamma/alpha;
  double c = delta/alpha;
  double Q = (std::pow(a, 2) - 3.0*b)/9.0;
  double R = (2.0*std::pow(a, 3) - 9.0*a*b + 27.0*c)/54.0;

  if (std::pow(R, 2) < std::pow(Q, 3)) {
    double theta = std::acos(R/std::sqrt(std::pow(Q, 3)));
    y(0) = -2.0*std::sqrt(Q)*std::cos(theta/3.0) -  a/3.0;
    y(1) = -2.0*std::sqrt(Q)*std::cos((theta + 2.0*arma::datum::pi)/3.0) - a/3.0;
    y(2) = -2.0*std::sqrt(Q)*std::cos((theta - 2.0*arma::datum::pi)/3.0) - a/3.0;
  } else {
    double A = -sign(R)*std::cbrt(std::abs(R) + std::sqrt(pow(R, 2) -
                                                               std::pow(Q, 3)));
    double B = nearly_equal(A, 0.0) ? 0.0 : Q/A;
    y(0) = A + B - a/3.0;
    y(1) = -0.5*(A + B) - a/3.0 + std::sqrt(3.0)*i*(A - B)/2.0;
    y(2) = -0.5*(A + B) - a/3.0 - std::sqrt(3.0)*i*(A - B)/2.0;
  }
  return y;
}

// // Cubic solver from Richter-Gebert
// void cubicSolver(double alpha,
//                  double beta,
//                  double gamma,
//                  double delta,
//                  arma::cx_vec lambda,
//                  arma::cx_vec mu) {
//   double W = -2.0*std::pow(beta, 3) + 9.0*alpha*beta*gamma -
//     27.0*std::pow(alpha, 2)*delta;
//   double D = -std::pow(beta, 2)*std::pow(gamma, 2) +
//     4*alpha*std::pow(gamma, 3) + 4*std::pow(beta, 3)*delta -
//     18*alpha*beta*gamma*delta + 27*std::pow(alpha, 2)*std::pow(delta, 3);
//   std::complex<double> Q = W - alpha*std::sqrt(27.0*D);
//   std::complex<double> R = std::pow(4.0*Q, 1/3);
//
//   std::complex<double> i(0.0, 1.0);
//
//   std::complex<double> omega = -0.5 + i*std::sqrt(3.0/4.0);
//
//   arma::cx_vec::fixed<3> L;
//   L(0) = 2*std::pow(beta, 2) - 6*alpha*gamma;
//   L(1) = -beta;
//   L(2) = R;
//
//   arma::cx_vec::fixed<3> M;
//   M(0) = R;
//   M(1) = 1;
//   M(2) = 2;
//   M *= 3*alpha;
//
//   arma::cx_mat::fixed<3, 3> O;
//   O.ones();
//   O(0, 0) = omega;
//   O(2, 2) = omega;
//   O(0, 2) = std::pow(omega, 2);
//   O(2, 0) = std::pow(omega, 2);
//
//   lambda = O*L;
//   mu = O*M;
// }

#endif
