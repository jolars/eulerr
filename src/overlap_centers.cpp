// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::LogicalMatrix find_surrounding_sets(const arma::vec& x,
                                          const arma::vec& y,
                                          const arma::vec& h,
                                          const arma::vec& k,
                                          const arma::vec& a,
                                          const arma::vec& b,
                                          const arma::vec& phi) {
  arma::uword n = h.n_elem;
  arma::umat out(x.n_elem, n);

  for (arma::uword i = 0; i < n; i ++) {
    out.col(i) = arma::square((x - h(i))*std::cos(phi(i)) + (y - k(i))*std::sin(phi(i)))/std::pow(a(i), 2) +
                 arma::square((x - h(i))*std::sin(phi(i)) - (y - k(i))*std::cos(phi(i)))/std::pow(b(i), 2) < 1;
  }

  return Rcpp::wrap(out.t());
}


// The code below code is adapted from "Distance from a Point to an Ellipse, an
// Ellipsoid, or a Hyperellipsoid" by David Eberly, Geometric Tools, LLC
// (c) 1998-2016
// https://www.geometrictools.com/Documentation/DistancePointEllipseEllipsoid.pdf

const int max_it = std::numeric_limits<double>::digits -
  std::numeric_limits<double>::min_exponent;

// Bisect
double bisect(double r0,
              double z0,
              double z1,
              double g) {
  double n0 = r0*z0;
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
