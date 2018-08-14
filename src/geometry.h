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

#ifndef eulerr_geometry_
#define eulerr_geometry_

#include <RcppArmadillo.h>

using namespace arma;

// See if a group of ellipses are completely disjoint or a russian doll
inline
double
disjoint_or_subset(const arma::mat& M)
{
  rowvec areas = M.row(2)%M.row(3)*datum::pi;
  uword i = areas.index_min();

  rowvec xmh    = M(0, i) - M.row(0);
  rowvec ymk    = M(1, i) - M.row(1);
  rowvec phi    = M.row(4);
  rowvec cosphi = cos(phi);
  rowvec sinphi = sin(phi);

  urowvec is_subset = square(xmh%cosphi + ymk%sinphi)/square(M.row(2)) +
                      square(xmh%sinphi - ymk%cosphi)/square(M.row(3)) < 1.0;

  is_subset.shed_col(i);

  return all(is_subset) ? areas(i) : 0.0;
}


// find which among a family of ellipses contain the given points
inline
arma::umat
find_surrounding_sets(const rowvec& x,
                      const rowvec& y,
                      const rowvec& h,
                      const rowvec& k,
                      const rowvec& a,
                      const rowvec& b,
                      const rowvec& phi)
{
  umat out(h.n_elem, x.n_elem);

  for (uword i = 0; i < h.n_elem; ++i) {
    double cosphi = std::cos(phi(i));
    double sinphi = std::sin(phi(i));
    double hi = h(i);
    double ki = k(i);
    double ai = a(i);
    double bi = b(i);

    out.row(i) = square((x - hi)*cosphi + (y - ki)*sinphi)/(ai*ai) +
                 square((x - hi)*sinphi - (y - ki)*cosphi)/(bi*bi) < 1.0;
  }
  return out;
}

// See which ellipses contain a given set of points
inline
arma::umat
adopt(const mat& points,
      const mat& ellipses,
      const uword i,
      const uword j)
{
  umat out(ellipses.n_cols, 4);
  for (uword l = 0; l < ellipses.n_cols; ++l) {
    if ((l == i) || (l == j)) {
      out.row(l).ones();
    } else {
      rowvec x = points.row(0);
      rowvec y = points.row(1);
      double h = ellipses(0, l);
      double k = ellipses(1, l);
      double a = ellipses(2, l);
      double b = ellipses(3, l);
      double phi = ellipses(4, l);
      double cosphi = std::cos(phi);
      double sinphi = std::sin(phi);

      // Check if the points lie inside the ellipse
      out.row(l) = square((x - h)*cosphi + (y - k)*sinphi)/(a*a) +
                   square((x - h)*sinphi - (y - k)*cosphi)/(b*b) < 1.0;
    }
  }
  return out;
}

#endif
