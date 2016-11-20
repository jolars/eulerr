#include <Rcpp.h>
using namespace Rcpp;

IntegerVector order_(NumericVector x) {
  NumericVector sorted = clone(x).sort();
  return match(sorted, x);
}

LogicalVector in_(IntegerVector x, IntegerVector y) {
  LogicalVector z = match(x, y) > 0;
  z[is_na(z)] = false;
  return z;
}

// [[Rcpp::export]]
double more_area(NumericVector x_int, NumericVector y_int, NumericVector radiuses, IntegerMatrix circles) {
  //Sort points clockwise from center
  int n = x_int.size();
  NumericVector tan(n);
  double xv = sum(x_int) / n;
  double yv = sum(y_int) / n;

  for (int i = 0; i < n; i++) {
    tan[i] = std::atan2(x_int[i] - xv, y_int[i] - yv);
  }

  IntegerVector ind = order_(tan) - 1;

  // Reorder vectors and matrix
  x_int = x_int[ind];
  y_int = y_int[ind];

  int nrow = circles.nrow();
  int ncol = circles.ncol();
  IntegerMatrix mat(nrow, ncol);

  for (int i = 0; i < ncol; i++) {
    mat(_, i) = circles(_, ind[i]);
  }

  double area = 0;
  int j = n - 1;

  for (int i = 0; i < n; i++) {
    // Area of circle segment
    IntegerVector circle_now = mat(_, i);
    IntegerVector now = circle_now[in_(mat(_, i), mat(_, j))];
    double d = sqrt(pow((x_int[j] - x_int[i]), 2) + pow(y_int[j] - y_int[i], 2));
    NumericVector r = radiuses[now - 1];
    NumericVector u = 2 * asin(d / (2 * r));
    NumericVector a = (pow(r, 2) / 2) * (u - sin(u));
    area += min(a);

    // Area of triangular portion
    area += ((x_int[j] + x_int[i]) * (y_int[j] - y_int[i])) / 2;

    j = i;
  }
  return area;
}
