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

#ifndef eulerr_geometry_h_
#define eulerr_geometry_h_

#include <RcppArmadillo.h>
#include "ellipse.h"

using namespace arma;

// See if a group of ellipses are completely disjoint or a russian doll
inline
double
disjoint_or_subset(const std::vector<Ellipse>& ellipse,
                   const std::vector<int>& ind)
{
  std::vector<double> areas;
  areas.reserve(ind.size());

  for (auto i : ind) {
    areas.emplace_back(ellipse[i].area());
  }

  auto min_itr = std::min_element(areas.begin(), areas.end());
  int min_ind = ind[std::distance(areas.begin(), min_itr)];

  bool subset = false;

  double h0 = ellipse[min_ind].h;
  double k0 = ellipse[min_ind].k;

  for (const auto i : ind) {
    if (i != min_ind) {
      double h = ellipse[i].h;
      double k = ellipse[i].k;
      double a = ellipse[i].a;
      double b = ellipse[i].b;
      double phi = ellipse[i].phi;
      double cos_phi = std::cos(phi);
      double sin_phi = std::sin(phi);

      subset = std::pow((h0 - h)*cos_phi + (k0 - k)*sin_phi, 2)/(a*a) +
               std::pow((h0 - h)*sin_phi + (k0 - k)*cos_phi, 2)/(b*b) < 1.0;
      if (!subset)
        break;
    }
  }

  return subset ? *min_itr : 0.0;
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

inline
std::vector<int>
adopt2(const double x,
       const double y,
       const std::vector<Ellipse>& ellipses,
       const int a,
       const int b)
{
  auto n = ellipses.size();

  std::vector<int> out;
  out.reserve(n);

  for (int i = 0; i < n; ++i) {
    if ((i == a) || (i == b)) {
      out.emplace_back(i);
    } else {
      double h = ellipses[i].h;
      double k = ellipses[i].k;
      double a = ellipses[i].a;
      double b = ellipses[i].b;
      double phi = ellipses[i].phi;
      double cos_phi = std::cos(phi);
      double sin_phi = std::sin(phi);

      // Check if the points lie inside the ellipse
      bool inside = std::pow((x - h)*cos_phi + (y - k)*sin_phi, 2)/(a*a) +
                    std::pow((x - h)*sin_phi - (y - k)*cos_phi, 2)/(b*b) < 1.0;
      if (inside)
        out.emplace_back(i);
    }
  }

  out.shrink_to_fit();

  return out;
}

#endif // eulerr_geometry_h_
