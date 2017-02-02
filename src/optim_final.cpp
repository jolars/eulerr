// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Number of n choose k. (Credited to Ben Voigt.)
int nck(int n, int k) {
  if (k > n) return 0;
  if (k * 2 > n) k = n - k;
  if (k == 0) return 1;

  int result = n;

  for(int i = 2; i <= k; ++i) {
    result *= (n - i + 1);
    result /= i;
  }
  return result;
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
// [[Rcpp::export]]
arma::umat bit_index(int n) {
  int n_combos = 0;

  for (int i = 1; i < n + 1; i++) {
    n_combos += nck(n, i);
  }

  umat out(n_combos, n, fill::zeros);

  for (int i = 1, k = 0; i < n + 1; i++) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (int j = 0; j < n; ++j) {
        if (v[j]) {out(k, j) = true;}
      }
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }

  return out;
}

uvec set_intersect(const urowvec& x, const urowvec& y) {
  std::vector<int> out;
  std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
                        std::back_inserter(out));
  return conv_to<uvec>::from(out);
}

double disc_overlap(const vec& x, const vec& y, const vec& r) {
  double r1 = r(0);
  double r2 = r(1);
  double d = sqrt(pow(x(0) - x(1), 2) + pow(y(0) - y(1), 2));

  if (d >= r1 + r2) {
    // Disjoint
    return 0;

  } else if (d <= std::abs(r1 - r2)) {
    // Subset
    return datum::pi * pow(r.min(), 2);

  } else {
    // Intersecting
    double r1e = pow(r1, 2);
    double r2e = pow(r2, 2);
    double de = pow(d, 2);

    return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
      r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
      sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
  }
}

void get_intersections(mat& int_points,
                       umat& in_circles,
                       const vec& x,
                       const vec& y,
                       const vec& r) {
  uword n = x.n_elem;

  // Loop over all combinations of circles and their overlaps
  for (uword i = 0, k = 0; i < n - 1; i++) {
    for (uword j = i + 1; j < n; j++) {
      double d = sqrt(pow(x(i) - x(j), 2) + pow(y(i) - y(j), 2));

      if (d >= r(i) + r(j) || d <= std::abs(r(i) - r(j))) {
        // Shed rows of set combinations that do not intersect
        int_points.shed_rows(k, k + 1);
        in_circles.shed_rows(k, k + 1);
      } else {
        double l = (pow(r(i), 2) - pow(r(j), 2) + pow(d, 2)) / (2 * d);
        double h = sqrt(pow(r(i), 2) - pow(l, 2));
        double ld = l / d;
        double hd = h / d;
        double xd = x(j) - x(i);
        double yd = y(j) - y(i);

        int_points(k, 0) = xd * ld + yd * hd + x(i);
        int_points(k, 1) = yd * ld - xd * hd + y(i);
        in_circles(k, 0) = i;
        in_circles(k, 1) = j;
        k++;
        int_points(k, 0) = xd * ld - yd * hd + x(i);
        int_points(k, 1) = yd * ld + xd * hd + y(i);
        in_circles(k, 0) = i;
        in_circles(k, 1) = j;
        k++;
      }
    }
  }
  uword nrows = int_points.n_rows;

  for (uword i = 0, j = 0; i < nrows; i++) {
    // Check if each intersection point is inside all circles
    uvec in_all = (square(int_points(j, 0) - x) +
                   square(int_points(j, 1) - y)) <= square(r);

    // Make sure intersection points are coupled to their respective circles
    in_all(in_circles.row(j)).fill(true);

    if (!all(in_all)) {
      // Drop points that are not inside all circles
      int_points.shed_row(j);
      in_circles.shed_row(j);
    } else {
      j++;
    }
  }
}

double polyarc_areas(const mat& int_points,
                     const vec& r,
                     const umat& circles) {
  vec x_int = int_points.col(0);
  vec y_int = int_points.col(1);
  uword n = x_int.n_elem;

  // Sort points by their angle to the centroid
  uvec ind = sort_index(atan2(x_int - accu(x_int)/n, y_int - accu(y_int)/n));

  // Reorder vectors and matrix based on angles to centroid
  x_int = x_int(ind);
  y_int = y_int(ind);
  umat circles_ord = circles.rows(ind);
  double area = 0;

  for (uword i = 0, j = n - 1; i < n; i++) {
    // Circular segment
    double d = sqrt(pow(x_int(j) - x_int(i), 2) + pow(y_int(j) - y_int(i), 2));
    vec rr = r(set_intersect(circles_ord.row(i), circles_ord.row(j)));
    vec u = 2 * asin(d / (2 * rr));
    vec a = (u - sin(u)) % square(rr) / 2;

    // If we have two circles at these points, pick the smaller
    area += a.min();

    // Triangular segment
    area += ((x_int(j) + x_int(i)) * (y_int(j) - y_int(i))) / 2;
    j = i;
  }

  return area;
}

// [[Rcpp::export]]
arma::vec return_intersections(arma::vec par) {
  uword n_col = par.n_elem / 3;
  umat id = bit_index(n_col);
  uword n_row = id.n_rows;
  vec areas(n_row);
  vec x = par.head(n_col);
  vec y = par.subvec(n_col, n_col * 2 - 1);
  vec r = par.tail(n_col);

  for (uword i = 0; i < n_row; i++) {
    urowvec sets = id.row(i);
    uvec ids = find(sets == 1);
    uword n_sets = accu(sets);
    vec xx = x(ids);
    vec yy = y(ids);
    vec rr = r(ids);

    if (n_sets == 1) {
      // A single set
      areas(i) = datum::pi * pow(rr(0), 2);

    } else if (n_sets == 2) {
      // Two sets
      areas(i) = disc_overlap(xx, yy, rr);

    } else {
      // Three or more sets
      // Get all intersection points that are inside all circles
      uword n_pairs = n_sets * (n_sets - 1) / 2;
      mat int_points(2 * n_pairs, 2);
      umat in_circles(size(int_points));
      get_intersections(int_points, in_circles, xx, yy, rr);

      uword n_points = int_points.n_rows;

      if (n_points == 0) {
        // No overlaps, either disjoint or subset
        uvec is_subset = sqrt(square(xx(rr.index_min()) - xx) +
                              square(yy(rr.index_min()) - yy)) <=
                         abs(rr(rr.index_min()) - rr);
        is_subset(rr.index_min()) = true;

        if (all(is_subset)) {
          // One set is completely subset
          areas(i) = datum::pi * pow(rr.min(), 2);
        } else {
          // All sets are disjoint
          areas(i) = 0;
        }
      } else if (n_points < 3) {
        // Two circles overlapping inside the other circle(s)
        areas(i) = disc_overlap(xx(in_circles.row(0)),
                                yy(in_circles.row(0)),
                                rr(in_circles.row(0)));
      } else if (n_points > 2) {
        // Three or more circles overlapping
        areas(i) = polyarc_areas(int_points, rr, in_circles);
      }
    }
  }
  // Decompose unions into unique sections
  vec areas_out(n_row, fill::zeros);

  for (uword i = n_row; i --> 0; ) {
    umat subareas = id.cols(find(id.row(i) == true));
    uvec prev_areas = find(sum(subareas, 1) == subareas.n_cols);
    areas_out(i) = areas(i) - accu(areas_out(prev_areas));
  }

  return conv_to< std::vector<double> >::from(clamp(areas_out, 0, datum::inf));
}

// [[Rcpp::export]]
double venneuler_stress(const arma::vec& areas, const arma::vec& fit) {
  double sst = accu(square(fit));
  double slope = accu(areas % fit) / accu(square(areas));
  double sse = accu(square(fit - areas * slope));
  return sse / sst;
}

// [[Rcpp::export]]
double loss_final(const arma::vec par, const arma::vec areas) {
  // Sum of squared errors
  return accu(square(areas - return_intersections(par)));
}
