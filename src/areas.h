#include <RcppArmadillo.h>
#include "helpers.h"
#include "transformations.h"
#include "geometry.h"

// Area of an ellipse
inline
double
ellipse_area(const arma::vec& v) {
  return arma::datum::pi*v(2)*v(3);
}

inline
double
montecarlo(arma::mat ellipses) {
  double n = ellipses.n_cols;

  // Get bounding box for all the ellipses
  arma::rowvec h = ellipses.row(0);
  arma::rowvec k = ellipses.row(1);
  arma::rowvec a = ellipses.row(2);
  arma::rowvec b = ellipses.row(3);
  arma::rowvec phi = ellipses.row(4);

  // Sample points using Vogel's method
  arma::uword n_s = 10000;
  arma::rowvec seqn = arma::linspace<arma::rowvec>(0, n_s - 1, n_s);
  arma::rowvec theta = seqn*(arma::datum::pi*(3 - std::sqrt(5)));
  arma::rowvec rad = arma::sqrt(seqn/n_s);
  arma::mat p0(3, n_s);
  p0.row(0) = rad%arma::cos(theta);
  p0.row(1) = rad%arma::sin(theta);
  p0.row(2).ones();

  arma::vec areas(n);

  for (arma::uword i = 0; i < n; ++i) {
    // Fit the sampling points to the current ellipse
    arma::mat p1 = translate(h(i), k(i))*rotate(-phi(i))*scale(a(i), b(i))*p0;
    arma::umat in_which = find_surrounding_sets(p1.row(0), p1.row(1),
                                                h, k, a, b, phi);

    double inside = arma::accu(arma::all(in_which).t());

    // Update the area as the fraction of the points inside all ellipses to
    // the area of the ellipses
    areas(i) = (inside/n_s)*a(i)*b(i)*arma::datum::pi;
  }

  // Return the average of all the ellipses
  return arma::accu(areas)/n;
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
// Area of a sector
//
// Compute the area of an ellipse segment.
inline
double
ellipse_segment(const arma::vec& ellipse,
                const arma::vec& pa,
                const arma::vec& pb) {
  arma::vec::fixed<2> hk = ellipse.subvec(0, 1);
  double a = ellipse(2);
  double b = ellipse(3);
  double phi = ellipse(4);

  arma::vec::fixed<3> p0 = rotate(phi)*translate(-hk)*pa;
  arma::vec::fixed<3> p1 = rotate(phi)*translate(-hk)*pb;

  double x0 = p0(0);
  double x1 = p1(0);
  double y0 = p0(1);
  double y1 = p1(1);

  double theta0 = std::atan2(y0, x0);
  double theta1 = std::atan2(y1, x1);

  if (theta1 < theta0)
    theta1 += 2.0*arma::datum::pi;

  // Triangle part of the sector
  double triangle = 0.5*std::abs(x1*y0 - x0*y1);

  double dtheta = theta1 - theta0;

  if (dtheta <= arma::datum::pi) {
    // Sector area
    return sector_area(a, b, theta1) - sector_area(a, b, theta0) - triangle;
  } else {
    theta0 += 2.0*arma::datum::pi;
    //Sector area
    return a*b*arma::datum::pi - sector_area(a, b, theta0) +
      sector_area(a, b, theta1) + triangle;
  }
}

// Compute the area of a intersection of 2+ ellipses
inline
double
polysegments(arma::mat&& points,
             const arma::mat& ellipses,
             arma::umat&& parents,
             bool& failure) {
  arma::vec x_int = points.row(0).t();
  arma::vec y_int = points.row(1).t();
  arma::uword n = points.n_cols;

  // Sort points by their angle to the centroid
  arma::uvec ind = arma::sort_index(arma::atan2(x_int - arma::accu(x_int)/n,
                                                y_int - arma::accu(y_int)/n));

  // Reorder vectors and matrix based on angles to centroid
  points  = points.cols(ind);
  parents = parents.cols(ind);
  x_int   = x_int(ind);
  y_int   = y_int(ind);
  double area = 0.0;

  for (arma::uword i = 0, j = n - 1; i < n; ++i) {
    // First discover which ellipses the points belong to
    arma::uvec ii = set_intersect(parents.unsafe_col(i), parents.unsafe_col(j));
    arma::uword i_n = ii.n_elem;

    if (i_n > 0) {
      arma::vec areas(i_n);

      // Ellipse segment
      for (arma::uword k = 0; k < i_n; ++k)
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
