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

using namespace arma;

// Area of an ellipse
double
  ellipse_area(const arma::vec& v) {
    return datum::pi*v(2)*v(3);
  }

double
  montecarlo(arma::mat ellipses) {
    double n = ellipses.n_cols;

    // Get bounding box for all the ellipses
    rowvec h = ellipses.row(0);
    rowvec k = ellipses.row(1);
    rowvec a = ellipses.row(2);
    rowvec b = ellipses.row(3);
    rowvec phi = ellipses.row(4);

    // Sample points using Vogel's method
    uword n_s = 1e4;
    rowvec seqn = regspace<rowvec>(0, n_s - 1);
    rowvec theta = seqn*(datum::pi*(3 - std::sqrt(5)));
    rowvec rad = sqrt(seqn/n_s);
    mat p0(3, n_s);
    p0.row(0) = rad%cos(theta);
    p0.row(1) = rad%sin(theta);
    p0.row(2).ones();

    vec areas(n);

    for (uword i = 0; i < n; ++i) {
      // Fit the sampling points to the current ellipse
      mat p1 = translate(h(i), k(i))*rotate(-phi(i))*scale(a(i), b(i))*p0;
      umat in_which = find_surrounding_sets(p1.row(0), p1.row(1),
                                            h, k, a, b, phi);

      double inside = accu(all(in_which).t());

      // Update the area as the fraction of the points inside all ellipses to
      // the area of the ellipses
      areas(i) = (inside/n_s)*a(i)*b(i)*datum::pi;
    }

    // Return the average of all the ellipses
    return accu(areas)/n;
  }

// The code below is adapted from "The area of intersecting ellipses" by
// David Eberly, Geometric Tools, LLC (c) 1998-2016
//
// Area of an ellipse sector
inline
  double
  sector_area(const double a,
              const double b,
              const double theta) {
    return 0.5*a*b*(theta - std::atan2((b - a)*std::sin(2.0*theta),
                                       b + a + (b - a)*std::cos(2.0*theta)));
  }

// The code below is adapted from "The area of intersecting ellipses" by
// David Eberly, Geometric Tools, LLC (c) 1998-2016

// Compute the area of an ellipse segment.
double
  ellipse_segment(const arma::vec& ellipse,
                  const arma::vec& pa,
                  const arma::vec& pb) {
    vec::fixed<2> hk = ellipse.subvec(0, 1);
    double a = ellipse(2);
    double b = ellipse(3);
    double phi = ellipse(4);

    vec::fixed<3> p0 = rotate(phi)*translate(-hk)*pa;
    vec::fixed<3> p1 = rotate(phi)*translate(-hk)*pb;

    double x0 = p0(0);
    double x1 = p1(0);
    double y0 = p0(1);
    double y1 = p1(1);

    double theta0 = std::atan2(y0, x0);
    double theta1 = std::atan2(y1, x1);

    if (theta1 < theta0)
      theta1 += 2.0*datum::pi;

    // Triangle part of the sector
    double triangle = 0.5*std::abs(x1*y0 - x0*y1);

    double dtheta = theta1 - theta0;

    if (dtheta <= datum::pi) {
      // Sector area
      return sector_area(a, b, theta1) - sector_area(a, b, theta0) - triangle;
    } else {
      theta0 += 2.0*datum::pi;
      //Sector area
      return a*b*datum::pi - sector_area(a, b, theta0) +
        sector_area(a, b, theta1) + triangle;
    }
  }

// Compute the area of a intersection of 2+ ellipses
double
polysegments(arma::mat&& points,
             const arma::mat& ellipses,
             arma::umat&& parents,
             bool& failure) {
  vec x_int = points.row(0).t();
  vec y_int = points.row(1).t();
  uword n = points.n_cols;

  // Sort points by their angle to the centroid
  uvec ind = sort_index(atan2(x_int - accu(x_int)/n, y_int - accu(y_int)/n));

  // Reorder vectors and matrix based on angles to centroid
  points  = points.cols(ind);
  parents = parents.cols(ind);
  x_int   = x_int(ind);
  y_int   = y_int(ind);
  double area = 0.0;

  for (uword i = 0, j = n - 1; i < n; ++i) {
    // First discover which ellipses the points belong to
    uvec ii = set_intersect(parents.unsafe_col(i), parents.unsafe_col(j));

    if (ii.n_elem > 0) {
      vec areas(ii.n_elem);

      // Ellipse segment
      for (uword k = 0; k < ii.n_elem; ++k)
        areas(k) = ellipse_segment(ellipses.unsafe_col(ii(k)),
              points.unsafe_col(i),
              points.unsafe_col(j));

      // Triangular plus ellipse segment area
      area += 0.5*((x_int(j) + x_int(i))*(y_int(j) - y_int(i))) + areas.min();
    } else {
      // Emergency exit (and fallback) when algorithm fails
      failure = true;
      return 0.0;
    }

    j = i;
  }
  return area;
}
