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
#include "transformations.h"
#include "geometry.h"
#include "point.h"
#include "ellipse.h"

#ifndef eulerr_areas_h_
#define eulerr_areas_h_

double
montecarlo(const std::vector<Ellipse>& ellipses,
           const std::vector<int>&     indices)
{
  using namespace std;

  auto n = indices.size();

  vector<double> areas;
  areas.reserve(n);

  for (const auto ind : indices)
    areas.push_back(ellipses[ind].area());

  // pick the ellipse with the smallest area
  auto ind_min = indices[min_index(areas)];

  size_t n_points = 1e4;
  size_t n_inside = 0;

  for (size_t i = 0; i < n_points; ++i) {
    // Sample points using Vogel's method
    double theta = i*(PI*(3.0 - sqrt(5.0)));
    double r = sqrt(i/n_points);

    Point p{r*cos(theta), r*sin(theta)};

    // modify point to fit ellipse
    p.scale(ellipses[ind_min].a, ellipses[ind_min].b);
    p.rotate(ellipses[ind_min].phi);
    p.translate(ellipses[ind_min].h, ellipses[ind_min].k);

    // check if point is inside the intersection
    bool inside = true;

    for (const auto ind : indices) {
      if (ind == ind_min)
        continue;

      if (!point_in_ellipse(p, ellipses[ind]))
        break;
    }

    if (inside)
      n_inside++;
  }

  return (n_inside/n_points)*ellipses[ind_min].area();
}

// The code below is adapted from "The area of intersecting ellipses" by
// David Eberly, Geometric Tools, LLC (c) 1998-2016
//
// Area of an ellipse sector
inline
double
sector_area(const double a,
            const double b,
            const double theta)
{
  return 0.5*a*b*(theta - std::atan2((b - a)*std::sin(2.0*theta),
                                     b + a + (b - a)*std::cos(2.0*theta)));
}

// The code below is adapted from "The area of intersecting ellipses" by
// David Eberly, Geometric Tools, LLC (c) 1998-2016

// Compute the area of an ellipse segment.
double
ellipse_segment(const Ellipse& ellipse, Point p0, Point p1)
{
  double h = ellipse.h;
  double k = ellipse.k;
  double a = ellipse.a;
  double b = ellipse.b;
  double phi = ellipse.phi;

  p0.translate(-h, -k);
  p0.rotate(-phi);
  p1.translate(-h, -k);
  p1.rotate(-phi);

  double theta0 = std::atan2(p0.k, p0.h);
  double theta1 = std::atan2(p1.k, p1.h);

  if (theta1 < theta0)
    theta1 += 2.0*PI;

  // Triangle part of the sector
  double triangle = 0.5*std::abs(p1.h*p0.k - p0.h*p1.k);

  double dtheta = theta1 - theta0;

  if (dtheta <= PI) {
    // Sector area
    return sector_area(a, b, theta1) - sector_area(a, b, theta0) - triangle;
  } else {
    theta0 += 2.0*PI;
    //Sector area
    return a*b*PI - sector_area(a, b, theta0)
           + sector_area(a, b, theta1)
           + triangle;
  }
}

// Compute the area of an intersection of 2+ ellipses
double
polysegments(const std::vector<Point>&              points,
             const std::vector<Ellipse>&            ellipses,
             const std::vector<std::array<int, 2>>& parents,
             std::vector<int>                       int_points,
             bool&                                  failure)
{
  auto n = int_points.size();

  // Sort points by their angle to the centroid
  double h0, k0 = 0.0;

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

  for (int k = 0, l = n - 1; k < n; ++k) {
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
        areas.emplace_back(ellipse_segment(ellipses[m],
                                           points[i],
                                           points[j]));

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
