// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "helpers.h"
#include "constants.h"
#include "transformations.h"

// [[Rcpp::export]]
arma::umat find_surrounding_sets(const arma::rowvec& x,
                                 const arma::rowvec& y,
                                 const arma::vec& h,
                                 const arma::vec& k,
                                 const arma::vec& a,
                                 const arma::vec& b,
                                 const arma::vec& phi) {
  arma::uword n = h.n_elem;
  arma::umat out(n, x.n_elem);

  for (arma::uword i = 0; i < n; ++i) {
    double cosphi = std::cos(phi(i));
    double sinphi = std::sin(phi(i));
    out.row(i) =
      arma::square((x - h(i))*cosphi + (y - k(i))*sinphi)/std::pow(a(i), 2) +
      arma::square((x - h(i))*sinphi - (y - k(i))*cosphi)/std::pow(b(i), 2) < 1;
  }

  return out;
}


// The code below code is adapted from "Distance from a Point to an Ellipse, an
// Ellipsoid, or a Hyperellipsoid" by David Eberly, Geometric Tools, LLC
// (c) 1998-2016
// https://www.geometrictools.com/Documentation/DistancePointEllipseEllipsoid.pdf

const int max_it = std::numeric_limits<double>::digits -
  std::numeric_limits<double>::min_exponent;

// Bisect
double bisect(const double r0,
              const double z0,
              const double z1,
              double g) {
  const double n0 = r0*z0;
  double s0 = z1 - 1;
  double s1 = g < 0 ? 0 : std::hypot(n0, z1) - 1;
  double s = 0;

  for (arma::uword i = 0; i < max_it; ++i) {
    s = (s0 + s1) / 2;
    if (s == s0 || s == s1) {
      break;
    } else {
      g = std::pow(n0/(s + r0), 2) + std::pow(z1/(s + 1), 2) - 1;
      if (g > 0) {
        s0 = s;
      } else if (g < 0) {
        s1 = s;
      } else {
        break;
      }
    }
  }
  return s;
}

// [[Rcpp::export]]
double dist_to_ellipse(double a,
                       double b,
                       double x,
                       double y) {

  // Flip the coordinate system if semi-major axis > semi-minor axis
  if (b > a) {
    std::swap(x, y);
    std::swap(a, b);
  }

  // Operate in the first quadrant only
  x = x < 0 ? -x : x;
  y = y < 0 ? -y : y;

  if (y > 0) {
    if (x > 0) {
      double z0 = x/a;
      double z1 = y/b;
      double g = std::pow(z0, 2) + std::pow(z1, 2) - 1;
      if (g != 0) {
        double r0 = std::pow(a/b, 2);
        double sbar = bisect(r0, z0, z1, g);
        return std::hypot(r0*x/(sbar + r0) - x, y/(sbar + 1) - y);
      } else {
        return 0;
      }
    } else {
      return std::abs(y - b);
    }
  } else {
    double numer0 = a*x;
    double denom0 = std::pow(a, 2) - std::pow(b, 2);
    if (numer0 < denom0) {
      double xda = numer0/denom0;
      return std::hypot(a*xda - x, b*std::sqrt(1 - std::pow(xda, 2)));
    } else {
      return std::abs(x - a);
    }
  }
}

// [[Rcpp::export]]
arma::uword max_colmins(const arma::mat& x) {
  arma::uword n = x.n_cols;
  arma::vec mins(n);
  for (arma::uword i = 0; i < n; i++)
    mins(i) = x.col(i).min();
  return mins.index_max();
}

// [[Rcpp::export]]
arma::mat locate_centers(const arma::vec& h,
                         const arma::vec& k,
                         const arma::vec& a,
                         const arma::vec& b,
                         const arma::vec& phi,
                         const arma::vec& orig,
                         const arma::vec& fitted) {
  arma::uword n = h.n_elem;
  arma::mat xyn;

  if (n > 1) {
    // Evenly space points across template circle
    arma::uword n_s = 500;
    arma::rowvec seqn = arma::linspace<arma::rowvec>(0, n_s - 1, n_s);
    arma::rowvec theta = seqn*(arma::datum::pi*(3 - std::sqrt(5)));
    arma::rowvec rad = arma::sqrt(seqn / n_s);
    arma::mat p0(3, n_s);
    p0.row(0) = rad % arma::cos(theta);
    p0.row(1) = rad % arma::sin(theta);
    p0.row(2).ones();

    arma::umat id = bit_index(n);
    arma::uword n_combos = id.n_rows;

    xyn.set_size(n_combos, 3);
    xyn.fill(arma::datum::nan);

    arma::uvec not_zero = fitted > small;
    arma::uvec singles = arma::sum(id, 1) == 1;

    for (arma::uword i = 0; i < n; i++) {
      // Fit the sampling points to the current ellipse
      arma::mat p1 = translate(h(i), k(i))*rotate(-phi(i))*scale(a(i), b(i))*p0;
      arma::umat in_which = find_surrounding_sets(p1.row(0),
                                                  p1.row(1),
                                                  h, k, a, b, phi);

      arma::uvec seqr = arma::find(id.col(i));

      for (auto j : seqr) {
        arma::uvec idj = id.row(j).t();
        if (xyn.row(j).has_nan() && idj(i)) {
          arma::urowvec locs(in_which.n_cols);
          if (singles(j)) {
            arma::urowvec sums = arma::sum(in_which);
            locs = sums == 1;
          } else {
            for (arma::uword f = 0; f < in_which.n_cols; f++) {
              locs(f) = arma::all(in_which.col(f) == idj);
            }
          }

          if (arma::any(locs)) {
            arma::mat p2 = p1.cols(arma::find(locs));
            arma::mat mm(n, p2.n_cols);
            for (arma::uword q = 0; q < n; q++) {
              arma::mat p3 = rotate(phi(q))*translate(-h(q), -k(q))*p2;
              for (arma::uword r = 0; r < p2.n_cols; r++) {
                mm(q, r) = dist_to_ellipse(a(q), b(q), p3(0, r), p3(1, r));
              }
            }
            arma::uword labmax = max_colmins(mm);
            xyn(j, 0) = p2(0, labmax);
            xyn(j, 1) = p2(1, labmax);
          }
        }
      }
    }
  } else {
    // One set, always placed in the middle
    xyn.set_size(1, 3);
    xyn.cols(1, 2).zeros();
  }

  xyn.col(2) = orig;

  return xyn;
}
