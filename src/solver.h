// eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
// Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
            const double delta)
{
  std::complex<double> i(0.0, 1.0);
  cx_vec::fixed<3> y;

  double a = beta/alpha;
  double b = gamma/alpha;
  double c = delta/alpha;
  double Q = (a*a - 3.0*b)/9.0;
  double R = (2.0*a*a*a - 9.0*a*b + 27.0*c)/54.0;

  if (R*R < Q*Q*Q) {
    double theta = std::acos(R/std::sqrt(Q*Q*Q));
    y(0) = -2.0*std::sqrt(Q)*std::cos(theta/3.0) -  a/3.0;
    y(1) = -2.0*std::sqrt(Q)*std::cos((theta + 2.0*datum::pi)/3.0) - a/3.0;
    y(2) = -2.0*std::sqrt(Q)*std::cos((theta - 2.0*datum::pi)/3.0) - a/3.0;
  } else {
    double A = -signum(R)*std::cbrt(std::abs(R) + std::sqrt(R*R - Q*Q*Q));
    double B = nearly_equal(A, 0.0) ? 0.0 : Q/A;
    y(0) = A + B - a/3.0;
    y(1) = -0.5*(A + B) - a/3.0 + std::sqrt(3.0)*i*(A - B)/2.0;
    y(2) = -0.5*(A + B) - a/3.0 - std::sqrt(3.0)*i*(A - B)/2.0;
  }
  return y;
}

#endif
