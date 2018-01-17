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

#ifndef eulerr_transformations_h_
#define eulerr_transformations_h_

#include <RcppArmadillo.h>

using namespace arma;

// Scaling matrix
inline
arma::mat
scale(const arma::vec& xy) {
  mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) = xy(0);
  out(1, 1) = xy(1);
  return out;
}

inline
arma::mat
scale(const double x,
      const double y) {
  mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) = x;
  out(1, 1) = y;
  return out;
}

// Translation matrix
inline
arma::mat
translate(const arma::vec& xy) {
  mat::fixed<3, 3> out;
  out.eye();
  out(span(0, 1), 2) = xy;
  return out;
}

inline
arma::mat
translate(const double x,
          const double y) {
  mat::fixed<3, 3> out;
  out.eye();
  out(0, 2) = x;
  out(1, 2) = y;
  return out;
}

// Produce a rotation matrix
inline
arma::mat
rotate(const double phi) {
  mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) =  std::cos(phi);
  out(1, 0) = -std::sin(phi);
  out(0, 1) =  std::sin(phi);
  out(1, 1) =  std::cos(phi);
  return out;
}

// Return the adjoint (adjugate) of a 3-by-3 matrix
inline
arma::mat
adjoint(const arma::mat& m) {
  mat::fixed<3, 3> out;
  const double a = m(0, 0);
  const double b = m(1, 0);
  const double c = m(1, 1);
  const double d = m(2, 0);
  const double e = m(2, 1);
  const double f = m(2, 2);

  out(0, 0) = c*f - e*e;
  out(1, 0) = d*e - b*f;
  out(1, 1) = a*f - d*d;
  out(2, 0) = b*e - c*d;
  out(2, 1) = b*d - a*e;
  out(2, 2) = a*c - b*b;

  return symmatl(out);
}

// Skew-symmetric matrix for complex matrices
inline
arma::cx_mat
skewsymmat(const arma::cx_vec& v) {
  cx_mat::fixed<3, 3> out;
  out.diag().zeros();
  out(0, 1) =  v(2);
  out(0, 2) = -v(1);
  out(1, 0) = -v(2);
  out(1, 2) =  v(0);
  out(2, 0) =  v(1);
  out(2, 1) = -v(0);
  return out;
}

// Skew-symmetric matrix
inline
arma::mat
skewsymmat(const arma::vec& v) {
  mat::fixed<3, 3> out;
  out.diag().zeros();
  out(0, 1) =  v(2);
  out(0, 2) = -v(1);
  out(1, 0) = -v(2);
  out(1, 2) =  v(0);
  out(2, 0) =  v(1);
  out(2, 1) = -v(0);
  return out;
}

#endif
