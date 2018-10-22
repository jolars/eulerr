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

//#define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "helpers.h"
#include "constants.h"
#include "transformations.h"
#include "neldermead.h"
#include "geometry.h"

// The code below code is adapted from "Distance from a Point to an Ellipse, an
// Ellipsoid, or a Hyperellipsoid" by David Eberly, Geometric Tools, LLC
// (c) 1998-2016
// https://www.geometrictools.com/Documentation/DistancePointEllipseEllipsoid.pdf

const int max_it = std::numeric_limits<double>::digits -
  std::numeric_limits<double>::min_exponent;

// Bisect
double
bisect(const double r0,
       const double z0,
       const double z1,
       double g)
{
  const double n0 = r0*z0;
  double s0 = z1 - 1;
  double s1 = g < 0.0 ? 0.0 : std::hypot(n0, z1) - 1.0;
  double s = 0.0;

  for (int i = 0; i < max_it; ++i) {
    s = (s0 + s1) / 2;
    if (s == s0 || s == s1) {
      break;
    } else {
      g = std::pow(n0/(s + r0), 2) + std::pow(z1/(s + 1.0), 2) - 1.0;
      if (g > 0.0) {
        s0 = s;
      } else if (g < 0.0) {
        s1 = s;
      } else {
        break;
      }
    }
  }
  return s;
}

// The code below code is adapted from "Distance from a Point to an Ellipse, an
// Ellipsoid, or a Hyperellipsoid" by David Eberly, Geometric Tools, LLC
// (c) 1998-2016
double
dist_to_ellipse(double a, double b, double x, double y)
{
  // Flip the coordinate system if semi-major axis > semi-minor axis
  if (b > a) {
    std::swap(x, y);
    std::swap(a, b);
  }

  // Operate in the first quadrant only
  if (x < 0.0)
    x = -x;
  if (y < 0.0)
    y = -y;

  if (y > 0.0) {
    if (x > 0.0) {
      double z0 = x/a;
      double z1 = y/b;
      double g = z0*z0 + z1*z1 - 1.0;
      if (g != 0.0) {
        double r0 = std::pow(a/b, 2);
        double sbar = bisect(r0, z0, z1, g);
        return std::hypot(r0*x/(sbar + r0) - x, y/(sbar + 1.0) - y);
      } else {
        return 0.0;
      }
    } else {
      return std::abs(y - b);
    }
  } else {
    double numer0 = a*x;
    double denom0 = a*a - b*b;
    if (numer0 < denom0) {
      double xda = numer0/denom0;
      return std::hypot(a*xda - x, b*std::sqrt(1.0 - xda*xda));
    } else {
      return std::abs(x - a);
    }
  }
}

double
dist_loss(const arma::vec& p,
          const arma::rowvec& h,
          const arma::rowvec& k,
          const arma::rowvec& a,
          const arma::rowvec& b,
          const arma::rowvec& phi)
{
  using namespace arma;

  auto n = h.n_elem;
  vec d(n);
  vec::fixed<3> pp;
  pp(span(0, 1)) = p;
  pp(2) = 1;

  for (uword i = 0; i < n; ++i) {
    vec::fixed<3> ppp = rotate(phi(i))*translate(-h(i), -k(i))*pp;
    d(i) = dist_to_ellipse(a(i), b(i), ppp(0), ppp(1));
  }
  return d.min();
}

// [[Rcpp::export]]
arma::mat
locate_centers(const arma::rowvec& h,
               const arma::rowvec& k,
               const arma::rowvec& a,
               const arma::rowvec& b,
               const arma::rowvec& phi,
               const arma::colvec& fitted)
{
  using namespace arma;

  uword n = h.n_elem;
  mat xy;

  if (n > 1) {
    // Evenly space points across template circle
    uword n_s = 1e3;
    rowvec seqn = linspace<rowvec>(0, n_s - 1, n_s);
    rowvec theta = seqn*(datum::pi*(3 - std::sqrt(5)));
    rowvec rad = sqrt(seqn/n_s);
    mat p0(3, n_s);
    p0.row(0) = rad%cos(theta);
    p0.row(1) = rad%sin(theta);
    p0.row(2).ones();

    umat id = bit_index(n);
    uword n_combos = id.n_rows;

    xy.set_size(2, n_combos);
    xy.fill(datum::nan);

    uvec not_zero = fitted > small;
    uvec singles = sum(id, 1) == 1;

    for (uword i = 0; i < n; ++i) {
      // Fit the sampling points to the current ellipse
      mat p1 = translate(h(i), k(i))*rotate(-phi(i))*scale(a(i), b(i))*p0;
      umat in_which = find_surrounding_sets(p1.row(0), p1.row(1),
                                                  h, k, a, b, phi);

      uvec seqr = find(id.unsafe_col(i));

      for (auto j : seqr) {
        uvec idj = id.row(j).t();
        if (xy.col(j).has_nan() && idj(i)) {
          urowvec locs(in_which.n_cols);
          if (singles(j)) {
            urowvec sums = sum(in_which);
            locs = sums == 1;
          } else {
            for (uword f = 0; f < in_which.n_cols; ++f) {
              locs(f) = all(in_which.unsafe_col(f) == idj);
            }
          }
          if (any(locs)) {
            mat p2 = p1.cols(find(locs));
            if (p2.n_cols != 0) {
              uword midpos = std::ceil(p2.n_cols/2);
              xy.col(j) = nelderMead(p2(span(0, 1), midpos),
                                     dist_loss, h, k, a, b, phi);
            }
          }
        }
      }
    }
  } else {
    // One set, always placed in the middle
    xy.set_size(2, 1);
    xy.zeros();
  }
  return xy;
}
