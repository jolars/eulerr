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

#ifndef eulerr_conversions_h_
#define eulerr_conversions_h_

#include "transformations.h"
#include "constants.h"

using namespace arma;

inline
arma::cube
standard_to_matrix(const mat& m)
{
  rowvec h = m.row(0);
  rowvec k = m.row(1);
  rowvec a = m.row(2);
  rowvec b = m.row(3);
  rowvec phi = m.row(4);

  rowvec A = square(a)%square(sin(phi)) + square(b)%square(cos(phi));
  rowvec B = 2.0*(square(b) - square(a))%sin(phi)%cos(phi);
  rowvec C = square(a)%square(cos(phi)) + square(b)%square(sin(phi));
  rowvec D = -2.0*A%h - B%k;
  rowvec E = -B%h - 2.0*C%k;
  rowvec F = A%square(h) + B%h%k + C%square(k) - square(a)%square(b);

  B *= 0.5;
  D *= 0.5;
  E *= 0.5;

  cube out(3, 3, m.n_cols);

  out.tube(0, 0) = A;
  out.tube(0, 1) = B;
  out.tube(0, 2) = D;
  out.tube(1, 0) = B;
  out.tube(1, 1) = C;
  out.tube(1, 2) = E;
  out.tube(2, 0) = D;
  out.tube(2, 1) = E;
  out.tube(2, 2) = F;

  out(find(abs(out) < small)).zeros();

  return out;
}

#endif
