// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

arma::uvec set_intersect(const arma::urowvec& x, const arma::urowvec& y) {
  std::vector<int> a = arma::conv_to<std::vector<int> >::from(arma::sort(x));
  std::vector<int> b = arma::conv_to<std::vector<int> >::from(arma::sort(y));
  std::vector<int> out;

  std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                        std::back_inserter(out));

  return arma::conv_to<arma::uvec>::from(out);
}

arma::uvec locate(const arma::uvec& x, const arma::uvec& y) {
  arma::uvec out(x.n_elem);

  arma::uvec::const_iterator it;
  arma::uvec::iterator out_it = out.begin();

  for(it = x.begin(); it != x.end(); ++it, ++out_it) {
    *out_it = any(y == *it);
  }

  return out;
}

// [[Rcpp::export]]
IntegerMatrix choose_two(IntegerVector x) {
  int n = x.size();
  IntegerMatrix m(n * (n - 1) / 2, 2);

  for (int i = 0, k = 0; i < n - 1; i++) {
    for (int j = i + 1; j < n; j++, k++) {
      m(k, 0) = x(i);
      m(k, 1) = x(j);
    }
  }

  return m;
}

// [[Rcpp::export]]
arma::mat intersect_all(const arma::vec& r1,
                        const arma::vec& r2,
                        const arma::vec& x_d,
                        const arma::vec& y_d,
                        const arma::vec& x_c,
                        const arma::vec& y_c,
                        const arma::vec& d) {
  arma::vec l = (square(r2) - square(r1) + square(d)) / (2 * d);
  arma::vec h = sqrt(square(r2) - square(l));
  arma::vec ld = l / d;
  arma::vec hd = h / d;

  arma::vec x1 = (x_d % ld) + (y_d % hd) + x_c;
  arma::vec x2 = (x_d % ld) - (y_d % hd) + x_c;
  arma::vec y1 = (y_d % ld) - (x_d % hd) + y_c;
  arma::vec y2 = (y_d % ld) + (x_d % hd) + y_c;

  return arma::join_rows(arma::join_cols(x1, x2), arma::join_cols(y1, y2));
}

// [[Rcpp::export]]
NumericVector discdisc(NumericVector r1, NumericVector r2, NumericVector d) {

  NumericVector r1e = pow(r1, 2);
  NumericVector r2e = pow(r2, 2);
  NumericVector de = pow(d, 2);

  return r1e * acos((de + r1e - r2e) / (2 * d * r1)) +
    r2e * acos((de + r2e - r1e) / (2 * d * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}

arma::vec discdisc_vec(const arma::vec& r1,
                       const arma::vec& r2,
                       const arma::vec& d) {
  arma::vec r1e = arma::square(r1);
  arma::vec r2e = arma::square(r2);
  arma::vec de = arma::square(d);

  return r1e % acos((de + r1e - r2e) / ((2 * d) % r1)) +
    r2e % acos((de + r2e - r1e) / ((2 * d) % r2)) -
    sqrt((r1 + r2 - d) % (d + r1 - r2) % (d - r1 + r2) % (d + r1 + r2)) / 2;
}

double discdisc_dbl(double r1, double r2, double d) {
  double r1e = pow(r1, 2);
  double r2e = pow(r2, 2);
  double de = pow(d, 2);

  return r1e * acos((de + r1e - r2e) / ((2 * d) * r1)) +
    r2e * acos((de + r2e - r1e) / ((2 * d) * r2)) -
    sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2;
}

arma::vec subv(const arma::vec& x, const arma::uvec& index) {
  arma::vec out(index.n_elem);

  arma::uvec::const_iterator it;
  arma::vec::iterator out_it = out.begin();

  for (it = index.begin(); it != index.end(); ++it, ++out_it) {
    *out_it = x(*it);
  }

  return out;
}

double polyarc_areas(arma::vec x_int,
                     arma::vec y_int,
                     const arma::vec& radiuses,
                     const arma::umat& circles) {

  // Sort points by their angle to the centroid
  uword n = x_int.n_elem;
  double xv = accu(x_int) / n;
  double yv = accu(y_int) / n;

  uvec ind = sort_index(atan2(x_int - xv, y_int - yv));

  // Reorder vectors and matrix based on angles to centroid
  x_int = x_int(ind);
  y_int = y_int(ind);
  umat matty = circles.rows(ind);

  double area = 0;

  for (arma::uword i = 0, j = n - 1; i < n; i++) {

    // Circular segment

    double d = sqrt(pow(x_int(j) - x_int(i), 2) + pow(y_int(j) - y_int(i), 2));
    arma::vec r = radiuses(set_intersect(matty.row(i), matty.row(j)));

    arma::vec u = 2 * asin(d / (2 * r));
    arma::vec a = (u - sin(u)) % square(r) / 2;

    // If we have two circles at these points, pick the smaller
    area += min(a);

    // Triangular segment
    area += ((x_int(j) + x_int(i)) * (y_int(j) - y_int(i))) / 2;

    j = i;
  }

  return area;

}

// [[Rcpp::export]]
std::vector<double> return_intersections(const arma::vec& par,
                                         const arma::umat& id,
                                         arma::umat two,
                                         const arma::uvec& twos,
                                         const arma::uvec& ones) {
  arma::uword N = par.n_elem;
  arma::uword n = id.n_cols;
  arma::uword n_combos = id.n_rows;

  arma::vec areas(n_combos);
  two = two - 1;

  arma::vec x = par.head(N / 3);
  arma::vec y = par.subvec(N / 3, N * 2 / 3 - 1);
  arma::vec r = par.tail(N / 3);

  arma::uvec two_a = two.col(0);
  arma::uvec two_b = two.col(1);

  arma::vec xa = subv(x, two_a);
  arma::vec xb = subv(x, two_b);
  arma::vec ya = subv(y, two_a);
  arma::vec yb = subv(y, two_b);
  arma::vec ra = subv(r, two_a);
  arma::vec rb = subv(r, two_b);

  arma::vec x_d = xa - xb;
  arma::vec y_d = ya - yb;
  arma::vec d = sqrt(arma::square(x_d) + arma::square(y_d));

  arma::uvec contained = d <= abs(ra - rb);
  arma::uvec disjoint = d >= (ra + rb);
  arma::uvec intersecting = (disjoint + contained) < 1;

  arma::uvec itwos = arma::find(twos == 1);
  arma::vec atwos = areas(itwos);

  arma::uvec ct = arma::find(contained == 1);
  arma::uvec dj = arma::find(disjoint == 1);
  arma::uvec is = arma::find(intersecting == 1);

  atwos.elem(ct) = arma::square(min(ra(ct), rb(ct))) * datum::pi;
  atwos.elem(dj).zeros();
  atwos.elem(is) = discdisc_vec(ra(is), rb(is), d(is));

  arma::mat int_points = intersect_all(ra, rb, x_d, y_d, xb, yb, d);

  arma::uword nint = int_points.n_rows;
  arma::umat in_circles(nint, n);

  for (arma::uword i = 0; i < n; i++) {
    in_circles.col(i) = (pow(int_points.col(0) - x(i), 2) +
      pow(int_points.col(1) - y(i), 2)) <= pow(r(i), 2);
  }

  arma::umat twoway = arma::join_cols(two.rows(is), two.rows(is));

  arma::uvec isis = arma::find(arma::join_cols(intersecting,
                                               intersecting) == 1);

  arma::umat in_circles_is = in_circles.rows(isis);

  for (arma::uword i = 0; i < twoway.n_rows; i++) {
    in_circles_is(i, twoway(i, 0)) = 1;
    in_circles_is(i, twoway(i, 1)) = 1;
  }

  in_circles.rows(isis) = in_circles_is;

  arma::umat all_circles = arma::join_cols(two, two);

  areas(arma::find(ones == 1)) = arma::square(r) * datum::pi;
  areas(itwos) = atwos;

  // Work out areas of relationships of 3 or more sets.
  for (arma::uword i = n * (n - 1) / 2 + n; i < n_combos; i++) {

    arma::uvec curr_set = arma::find(id.row(i) == 1);
    arma::uvec twoway = locate(two_a, curr_set) && locate(two_b, curr_set);

    arma::uvec twotwo = arma::join_cols(twoway, twoway);
    arma::uvec in_all =
      arma::sum(in_circles.cols(curr_set), 1) == curr_set.n_elem;

    in_all = in_all && twotwo;

    arma::uvec veci = arma::find(in_all == 1);

    arma::umat circles = all_circles.rows(veci);

    arma::uword crow = circles.n_rows;

    if (crow < 2) {

      arma::uword l = curr_set(r(curr_set).index_min());
      arma::uvec la(1);
      la.fill(l);

      arma::uvec ff = locate(two_a, la) || locate(two_b, la);

      if (all(contained(arma::find(ff == 1)))) {

        // One set is contained within the others. Return its area.
        areas(i) = pow(min(r(curr_set)), 2) * datum::pi;

      } else {

        // All sets are disjoint. Return 0.
        areas(i) = 0;

      }
    } else if (crow == 2) {

      // Return 2 circle intersection
      areas(i) = discdisc_dbl(r(circles(0, 0)), r(circles(0, 1)), d(veci(0)));

    } else {

      // Return intersection of 3 or more circles
      arma::mat curr_intpoints = int_points.rows(veci);
      areas(i) = polyarc_areas(curr_intpoints.col(0),
                               curr_intpoints.col(1),
                               r, circles);
    }
  }

  vec areas_cut(areas.n_elem);

  // Figure out intersections and relative complements
  for (unsigned int i = areas.size(); i-- > 0; ) {

    urowvec idx = id.row(i);

    umat subareas = id.cols(find(idx == 1));
    uvec prev_areas = find(sum(subareas, 1) == subareas.n_cols);
    areas_cut(i) = areas(i) - accu(areas_cut(prev_areas));

  }

  areas_cut.replace(datum::nan, 0);
  arma::vec areas_out = arma::clamp(areas_cut, 0, 1);

  return arma::conv_to< std::vector<double> >::from(areas_out);

}

// [[Rcpp::export]]
double stress(const arma::vec& areas, const arma::vec& fit) {

  double sst = arma::accu(arma::square(fit));
  double slope = arma::accu(areas % fit) / arma::accu(arma::square(areas));
  double sse = arma::accu(arma::square(fit - areas * slope));

  return sse / sst;
}

// [[Rcpp::export]]
double compute_fit(const arma::vec par,
                   const arma::vec areas,
                   const arma::umat id,
                   const arma::umat two,
                   const arma::uvec twos,
                   const arma::uvec ones,
                   const arma::uword cost) {

  arma::vec fit = return_intersections(par, id, two, twos, ones);

  switch(cost) {

  case 0:
    // venneuler cost function
    return stress(areas, fit);

  case 1:
    // sums of squared errors
    return arma::accu(arma::square(fit - areas));

  default:
    // throw an error if we for some reason end up here
    stop("None of the cost functions were triggered. This might be a bug.");

  }
}
