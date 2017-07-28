// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include "helpers.h"

// [[Rcpp::export]]
arma::umat choose_two(const arma::uvec& x) {
  arma::uword n = x.size();
  arma::umat m(n * (n - 1) / 2, 2);
  for (arma::uword i = 0, k = 0; i < n - 1; i++) {
    for (arma::uword j = i + 1; j < n; j++, k++) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }
  return m;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix bit_indexr(const arma::uword n) {
  return Rcpp::wrap(bit_index(n));
}

// [[Rcpp::export]]
Rcpp::NumericVector discdisc(const Rcpp::NumericVector& r1,
                             const Rcpp::NumericVector& r2,
                             const Rcpp::NumericVector& d) {
  Rcpp::NumericVector r1e = Rcpp::pow(r1, 2);
  Rcpp::NumericVector r2e = Rcpp::pow(r2, 2);
  Rcpp::NumericVector de  = Rcpp::pow(d, 2);

  return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
    r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}

// [[Rcpp::export]]
Rcpp::LogicalMatrix find_surrounding_sets(const arma::vec& x,
                                          const arma::vec& y,
                                          const arma::vec& h,
                                          const arma::vec& k,
                                          const arma::vec& a,
                                          const arma::vec& b,
                                          const arma::vec& phi) {
  arma::uword n1 = h.n_elem;
  arma::uword n2 = x.n_elem;
  arma::umat out(n1, n2);

  for (arma::uword i = 0; i < n1; i++) {
    for (arma::uword j = 0; j < n2; j++) {
      out(i, j) =
        std::pow((x(j) - h(i))*std::cos(phi(i)) + (y(j) - k(i))*std::sin(phi(i)), 2)/
          std::pow(a(i), 2) +
        std::pow((x(j) - h(i))*std::sin(phi(i)) - (y(j) - k(i))*std::cos(phi(i)), 2)/
          std::pow(b(i), 2) < 1;
    }
  }

  return Rcpp::wrap(out);
}

// [[Rcpp::export]]
arma::uword max_colmins(const arma::mat& x) {
  arma::uword n = x.n_cols;
  arma::vec mins(n);
  for (arma::uword i = 0; i < n; i++)
    mins(i) = x.col(i).min();
  return mins.index_max() + 1;
}

// [[Rcpp::export]]
double venneuler_stress(const arma::vec& areas, const arma::vec& fit) {
  double sst   = arma::accu(arma::square(fit));
  double slope = arma::accu(areas % fit) / arma::accu(arma::square(areas));
  double sse   = arma::accu(arma::square(fit - areas * slope));
  return sse / sst;
}

const int max_it = std::numeric_limits<double>::digits - std::numeric_limits<double>::min_exponent;

double get_root(double r0,
                double z0,
                double z1,
                double g) {
  double n0 = r0*z0;
  double s0 = z1 - 1;
  arma::vec::fixed<2> n0z1;
  n0z1(0) = n0;
  n0z1(1) = z1;
  double s1 = g < 0 ? 0 : arma::norm(n0z1) - 1;
  double s = 0;

  for (arma::uword i = 0; i < max_it; ++i) {
    s = (s0 + s1) / 2;
    if (s == s0 || s == s1) {
      break;
    } else {
      double ratio0 = n0/(s + r0);
      double ratio1 = z1/(s + 1);
      g = std::pow(ratio0, 2) + std::pow(ratio1, 2) - 1;
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

double dist_to_ellipse(double e0,
                       double e1,
                       double y0,
                       double y1,
                       double x0,
                       double x1) {
  double distance;
  if (y1 > 0) {
    if (y0 > 0) {
      double z0 = y0/e0;
      double z1 = y1/e1;
      double g = std::pow(z0, 2) + std::pow(z1, 2) - 1;
      if (g != 0) {
        double r0 = std::pow(e0/e1, 2);
        double sbar = get_root(r0, z0, z1, g);
        x0 = r0*y0/(sbar + r0);
        x1 = y1/(sbar + 1);
        distance = std::sqrt(std::pow(x0 - y0, 2) + std::pow(x1 - y1, 2));
      } else {
        x0 = y0;
        x1 = y1;
        distance = 0;
      }
    } else {
      x0 = 0;
      x1 = e1;
      distance = std::abs(y1 - e1);
    }
  } else {
    double numer0 = e0*y0;
    double denom0 = std::pow(e0, 2) - std::pow(e1, 2);
    if (numer0 < denom0) {
      double xde0 = numer0/denom0;
      x0 = e0*xde0;
      x1 = e1*std::sqrt(1 - std::pow(xde0, 2));
      distance = std::sqrt(std::pow(x0 - y0, 2) + std::pow(x1, 2));
    } else {
      x0 = e0;
      x1 = 0;
      distance = std::abs(y0 - e0);
    }
  }

  return distance;
}


// func pointOnEllipse(center: CGPoint, a: CGFloat, b: CGFloat, closestTo p: CGPoint) -> CGPoint {
//
//   let maxIterations = 10
//   let eps = CGFloat(0.1/max(a, b))
//
//   let p1 = CGPoint(x: p.x - center.x, y: p.y - center.y)
//
//   // Intersection of straight line from origin to p with ellipse
//   // as the first approximation:
//   var phi = atan2(a * p1.y, b * p1.x)
//
//   // Newton iteration to find solution of
//   // f(θ) := (a^2 − b^2) cos(phi) sin(phi) − x a sin(phi) + y b cos(phi) = 0:
//   for i in 0..<maxIterations {
//     // function value and derivative at phi:
//     let (c, s) = (cos(phi), sin(phi))
//     let f = (a*a - b*b)*c*s - p1.x*a*s + p1.y*b*c
//     let f1 = (a*a - b*b)*(c*c - s*s) - p1.x*a*c - p1.y*b*s
//
//     let delta = f/f1
//     phi = phi - delta
//     print(i)
//     if abs(delta) < eps { break }
//   }
//
//   return CGPoint(x: center.x + a * cos(phi), y: center.y + b * sin(phi))
// }
// let p2 = pointOnEllipse(a: a, b: b, closestTo: p)
// let v = CGVector(dx: p.x - p2.x, dy: p.y - p2.y)
// let distance = hypot(v.dx, v.dy)
