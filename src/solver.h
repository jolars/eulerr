// Copyright Emanuel Huber
// This code has been ported to C++ from RConics version 1.0, which is licensed
// under the GPL-3 license.

#ifndef eulerr_solver_h_
#define eulerr_solver_h_

#include <RcppArmadillo.h>
#include "constants.h"
#include "helpers.h"

// Solve a cubic polynomial
arma::cx_vec solve_cubic(const arma::vec& v) {
  std::complex<double> i(0.0, 1.0);
  arma::cx_vec::fixed<3> y;

  double a = v(1)/v(0);
  double b = v(2)/v(0);
  double c = v(3)/v(0);
  double Q = (std::pow(a, 2) - 3.0*b)/9.0;
  double R = (2.0*std::pow(a, 3) - 9.0*a*b + 27.0*c)/54.0;

  if (std::pow(R, 2) < std::pow(Q, 3)) {
    double theta = std::acos(R/std::sqrt(std::pow(Q, 3)));
    y(0) = -2.0*std::sqrt(Q)*std::cos(theta/3.0) -  a/3.0;
    y(1) = -2.0*std::sqrt(Q)*std::cos((theta + 2.0*arma::datum::pi)/3.0) - a/3.0;
    y(2) = -2.0*std::sqrt(Q)*std::cos((theta - 2.0*arma::datum::pi)/3.0) - a/3.0;
  } else {
    double A =
      -sign(R)*std::cbrt(std::abs(R) + std::sqrt(pow(R, 2) - std::pow(Q, 3)));
    double B = nearly_equal(A, 0.0) ? 0.0 : Q/A;
    y(0) = A + B - a/3.0;
    y(1) = -0.5*(A + B) - a/3.0 + std::sqrt(3.0)*i*(A - B)/2.0;
    y(2) = -0.5*(A + B) - a/3.0 - std::sqrt(3.0)*i*(A - B)/2.0;
  }
  return y;
}

#endif
