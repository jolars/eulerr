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
Rcpp::LogicalMatrix find_surrounding_sets(const arma::vec& xs,
                                          const arma::vec& ys,
                                          const arma::vec& x,
                                          const arma::vec& y,
                                          const arma::vec& r) {
  arma::uword n1 = x.n_elem;
  arma::uword n2 = xs.n_elem;
  arma::umat out(n1, n2);

  for (arma::uword i = 0; i < n1; i++) {
    double r2 = std::pow(r(i), 2);
    for (arma::uword j = 0; j < n2; j++) {
      out(i, j) = (std::pow(xs(j) - x(i), 2) + std::pow(ys(j) - y(i), 2)) <= r2;
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


// [[Rcpp::export]]
double closest_point(double a,
                     double b,
                     double x,
                     double y) {

  double eps = 0.1/std::max(a, b);

  // Intersection of straight line from origin to p with ellipse
  // as the first approximation:
  double theta = std::atan2(a*y, b*x);

  double delta = std::max(a, b);
  arma::uword i = 0;

  while (std::abs(delta) > eps && i < 10) {
    // function value and derivative at phi:
    double c = std::cos(theta);
    double s = std::sin(theta);
    double f0 = (std::pow(a, 2) - std::pow(b, 2))*c*s - x*a*s + y*b*c;
    double f1 = (std::pow(a, 2) - std::pow(b, 2))*
      (std::pow(c, 2) - std::pow(s, 2)) - x*a*c - y*b*s;
    double delta = f0/f1;
    theta -= delta;

    ++i;
  }

  double xx = a*std::cos(theta);
  double yy = b*std::sin(theta);

  Rcpp::Rcout << xx << yy << std::endl;

  return std::hypot(x - xx, y - yy);
}


