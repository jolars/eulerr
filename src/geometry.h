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
#include "point.h"

inline
bool
point_in_ellipse(const eulerr::Point& p, const eulerr::Ellipse& e)
{
  using namespace std;

  return
    pow((p.h - e.h)*cos(e.phi) + (p.k - e.k)*sin(e.phi), 2)/(e.a*e.a) +
    pow((p.h - e.h)*sin(e.phi) - (p.k - e.k)*cos(e.phi), 2)/(e.b*e.b) <= 1.0;
}

// See if a group of ellipses are completely disjoint or a russian doll
inline
double
disjoint_or_subset(const std::vector<eulerr::Ellipse>& ellipse,
                   const std::vector<int>&             ind)
{
  std::vector<double> areas;
  areas.reserve(ind.size());

  for (auto i : ind)
    areas.emplace_back(ellipse[i].area());

  auto min_itr = std::min_element(areas.begin(), areas.end());
  auto min_ind = ind[std::distance(areas.begin(), min_itr)];

  eulerr::Point p{ellipse[min_ind].h, ellipse[min_ind].k};

  bool subset = false;

  for (const auto i : ind) {
    if (i != min_ind) {

      subset = point_in_ellipse(p, ellipse[i]);

      if (!subset)
        break;
    }
  }

  return subset ? *min_itr : 0.0;
}

inline
std::vector<int>
adopt(const eulerr::Point&                p,
      const std::vector<eulerr::Ellipse>& ellipses,
      const int                           a,
      const int                           b)
{
  int n = ellipses.size();

  std::vector<int> out;
  out.reserve(n);

  for (decltype(n) i = 0; i < n; ++i)
    if ((i == a) || (i == b) || point_in_ellipse(p, ellipses[i]))
      out.emplace_back(i);

  out.shrink_to_fit();

  return out;
}

#endif // eulerr_geometry_h_
