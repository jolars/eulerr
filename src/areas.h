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

#include <RcppArmadillo.h>
#include "helpers.h"
#include "geometry.h"
#include "point.h"
#include "ellipse.h"

#ifndef eulerr_areas_h_
#define eulerr_areas_h_

double
montecarlo(const std::vector<eulerr::Ellipse>& ellipses,
           const std::vector<int>&             indices)
{
  using namespace std;

  auto n = indices.size();

  vector<double> areas;
  areas.reserve(n);

  size_t n_points = 1e4;

  for (auto ind : indices) {

    size_t n_inside = 0;

    const auto& e = ellipses[ind];

    for (size_t i = 0; i < n_points; ++i) {
      // sample points using Vogel's method
      double theta = i*(PI*(3.0 - sqrt(5.0)));
      double r = sqrt(static_cast<double>(i)/static_cast<double>(n_points));

      eulerr::Point p{r*std::cos(theta), r*std::sin(theta)};

      // modify point to fit ellipse
      p.scale(e.a, e.b);
      p.rotate(e.phi);
      p.translate(e.h, e.k);

      // check if point is inside the intersection
      auto all_inside = all_of(
        indices.begin(),
        indices.end(),
        [&p, &ellipses, &ind](int i) {
          return i == ind || point_in_ellipse(p, ellipses[i]);
        }
      );

      if (all_inside)
        n_inside++;
    }

    areas.push_back(e.area()*n_inside/n_points);
  }

  return std::accumulate(areas.begin(), areas.end(), 0.0)/n;
}

// The code below is adapted from "The area of intersecting ellipses" by
// David Eberly, Geometric Tools, LLC (c) 1998-2016

// Compute the area of an ellipse segment.
double
ellipse_segment(const eulerr::Ellipse& e, eulerr::Point p0, eulerr::Point p1)
{
  p0.translate(-e.h, -e.k);
  p0.rotate(-e.phi);
  p1.translate(-e.h, -e.k);
  p1.rotate(-e.phi);

  double theta0 = std::atan2(p0.k, p0.h);
  double theta1 = std::atan2(p1.k, p1.h);

  if (theta1 < theta0)
    theta1 += 2.0*PI;

  // Triangle part of the sector
  double triangle = 0.5*std::abs(p1.h*p0.k - p0.h*p1.k);

  return
    (theta1 - theta0) <= PI ? e.sector(theta1) - e.sector(theta0) - triangle
                            : e.area()
                              - e.sector(theta0 + 2.0*PI)
                              + e.sector(theta1)
                              + triangle;
}

// Compute the area of an intersection of 2+ ellipses
double
polysegments(const std::vector<eulerr::Point>&      points,
             const std::vector<eulerr::Ellipse>&    ellipses,
             const std::vector<std::array<int, 2>>& parents,
             const std::vector<int>&                int_points,
             bool&                                  failure)
{
  auto n = int_points.size();

  // Sort points by their angle to the centroid
  double h0 = 0.0;
  double k0 = 0.0;

  for (auto i : int_points) {
    h0 += points[i].h/n;
    k0 += points[i].k/n;
  }

  std::vector<double> angle;
  angle.reserve(n);

  for (const auto i : int_points)
    angle.emplace_back(std::atan2(points[i].h - h0, points[i].k - k0));

  auto ind = seq(n);

  std::sort(ind.begin(), ind.end(),
            [&angle](int i, int j) { return angle[i] < angle[j]; });

  // Reorder vectors and matrix based on angles to centroid
  double area = 0.0;

  for (decltype(n) k = 0, l = n - 1; k < n; ++k) {
    auto i = int_points[ind[k]];
    auto j = int_points[ind[l]];

    // First discover which ellipses the points belong to
    std::vector<int> ii;

    std::set_intersection(std::begin(parents[i]), std::end(parents[i]),
                          std::begin(parents[j]), std::end(parents[j]),
                          std::back_inserter(ii));

    if (!ii.empty()) {
      std::vector<double> areas;
      areas.reserve(2);

      // Ellipse segment
      for (auto m : ii)
        areas.emplace_back(ellipse_segment(ellipses[m], points[i], points[j]));

      // Triangular plus ellipse segment area
      area += 0.5*((points[j].h + points[i].h)*(points[j].k - points[i].k))
              + *std::min_element(areas.begin(), areas.end());

    } else {
      // Emergency exit (and fallback) when algorithm fails
      failure = true;
      return 0.0;
    }

    l = k;
  }
  return area;
}

#endif // eulerr_areas_h_
