// Copyright Emanuel Huber
// This code has been ported to C++ from RConics version 1.0, which is licensed
// under the GPL-3 license.

#ifndef eulerr_solver_h_
#define eulerr_solver_h_

#include <RcppArmadillo.h>
#include "constants.h"
#include "helpers.h"

// Solve a cubic polynomial
inline
arma::cx_vec
solve_cubic(const double alpha,
            const double beta,
            const double gamma,
            const double delta) {
  std::complex<double> i(0.0, 1.0);
  arma::cx_vec::fixed<3> y;

  double a = beta/alpha;
  double b = gamma/alpha;
  double c = delta/alpha;
  double Q = (a*a - 3.0*b)/9.0;
  double R = (2.0*a*a*a - 9.0*a*b + 27.0*c)/54.0;

  if (R*R < Q*Q*Q) {
    double theta = std::acos(R/std::sqrt(Q*Q*Q));
    y(0) = -2.0*std::sqrt(Q)*std::cos(theta/3.0) -  a/3.0;
    y(1) = -2.0*std::sqrt(Q)*std::cos((theta + 2.0*arma::datum::pi)/3.0) - a/3.0;
    y(2) = -2.0*std::sqrt(Q)*std::cos((theta - 2.0*arma::datum::pi)/3.0) - a/3.0;
  } else {
    double A = -sign(R)*std::cbrt(std::abs(R) + std::sqrt(R*R - Q*Q*Q));
    double B = nearly_equal(A, 0.0) ? 0.0 : Q/A;
    y(0) = A + B - a/3.0;
    y(1) = -0.5*(A + B) - a/3.0 + std::sqrt(3.0)*i*(A - B)/2.0;
    y(2) = -0.5*(A + B) - a/3.0 - std::sqrt(3.0)*i*(A - B)/2.0;
  }
  return y;
}

#endif
