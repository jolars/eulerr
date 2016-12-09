#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

arma::uvec set_intersect(arma::uvec x, arma::uvec y) {

  std::vector<int> a = arma::conv_to< std::vector<int> >::from(x);
  std::vector<int> b = arma::conv_to< std::vector<int> >::from(y);
  std::vector<int> out;

  std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                        std::back_inserter(out));

  return arma::conv_to< arma::uvec >::from(out);
}

arma::uvec locate(arma::uvec x, arma::uvec y) {
  arma::uvec out(x.n_elem);

  arma::uvec::iterator it;
  arma::uvec::iterator out_it;

  for(it = x.begin(), out_it = out.begin(); it != x.end(); ++it, ++out_it) {
    *out_it = any(y == *it);
  }

  return out;
}

// [[Rcpp::export]]
arma::umat choose_two(arma::uvec x) {
  arma::uword n = x.n_elem;
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
arma::mat intersect_all(arma::vec r1, arma::vec r2,
                        arma::vec x_d, arma::vec y_d,
                        arma::vec x_c, arma::vec y_c,
                        arma::vec d) {
  arma::vec l = (pow(r2, 2) - pow(r1, 2) + pow(d, 2)) / (2 * d);
  arma::vec h = sqrt(pow(r2, 2) - pow(l, 2));
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

arma::vec discdisc_vec(arma::vec r1, arma::vec r2, arma::vec d) {
  arma::vec r1e = pow(r1, 2);
  arma::vec r2e = pow(r2, 2);
  arma::vec de = pow(d, 2);

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

// [[Rcpp::export]]
arma::vec subv(arma::vec x, arma::uvec index) {
  arma::vec out(index.n_elem);

  arma::uvec::iterator it;
  arma::vec::iterator out_it;

  for (it = index.begin(), out_it = out.begin(); it != index.end(); ++it, ++out_it) {
    *out_it = x(*it);
  }

  return out;
}

// [[Rcpp::export]]
double polyarc_areas(arma::vec x_int, arma::vec y_int, arma::vec radiuses,
                     arma::umat circles) {

  // Sort points by their angle to the centroid
  arma::uword n = x_int.n_elem;
  double xv = sum(x_int) / n;
  double yv = sum(y_int) / n;

  arma::vec srt_ind(n);
  for (arma::uword i = 0; i < n; i++) {
    srt_ind(i) = std::atan2(x_int(i) - xv, y_int(i) - yv);
  }

  arma::uvec ind = sort_index(srt_ind);

  // Reorder vectors and matrix based on angles to centroid
  x_int = x_int(ind);
  y_int = y_int(ind);
  arma::umat matty = circles.rows(ind);

  double area = 0;

  for (arma::uword i = 0, j = n - 1; i < n; i++) {

    // Circular segment
    arma::uvec now = set_intersect(matty.row(i).t(), matty.row(j).t());

    double d = sqrt(pow(x_int(j) - x_int(i), 2) + pow(y_int(j) - y_int(i), 2));
    arma::vec r = radiuses(now);

    arma::vec u = 2 * asin(d / (2 * r));
    arma::vec a = (u - sin(u)) % pow(r, 2) / 2;

    area += min(a);

    // Triangular segment
    area += ((x_int(j) + x_int(i)) * (y_int(j) - y_int(i))) / 2;

    j = i;
  }
  return area;
}


// [[Rcpp::export]]
std::vector<double> return_intersections(arma::vec par, arma::vec areas, arma::umat id,
                               arma::umat two, arma::uvec twos,
                               arma::uvec ones) {
  arma::uword N = par.n_elem;
  arma::uword n = id.n_cols;
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
  arma::vec d = sqrt(pow(x_d, 2) + pow(y_d, 2));

  arma::uvec contained = d <= abs(ra - rb);
  arma::uvec disjoint = d >= (ra + rb);
  arma::uvec intersecting = (disjoint + contained) == 0;

  arma::uvec itwos = arma::find(twos == 1);
  arma::vec atwos = areas(find(twos == 1));

  arma::uvec ct = arma::find(contained == 1);
  arma::uvec dj = arma::find(disjoint == 1);
  arma::uvec is = arma::find(intersecting == 1);

  atwos.elem(ct) = pow(min(ra(ct), rb(ct)), 2) * datum::pi;
  atwos.elem(dj).zeros();
  atwos.elem(is) = discdisc_vec(ra(is), rb(is), d(is));

  arma::mat int_points = intersect_all(ra, rb, x_d, y_d, xb, yb, d);

  arma::uword nint = int_points.n_rows;
  arma::umat in_circles(nint, n);

  arma::vec rsq = pow(r, 2);

  for (arma::uword i = 0; i < n; i++) {
    in_circles.col(i) = (pow(int_points.col(0) - x(i), 2) +
      pow(int_points.col(1) - y(i), 2)) <= rsq(i);
  }

  arma::umat twoway = arma::join_cols(two.rows(is), two.rows(is));

  arma::uvec isis = arma::find(arma::join_cols(intersecting, intersecting) == 1);

  arma::umat in_circles_is = in_circles.rows(isis);

  for (arma::uword i = 0; i < twoway.n_rows; i++) {
    in_circles_is(i, twoway(i, 0)) = 1;
    in_circles_is(i, twoway(i, 1)) = 1;
  }

  in_circles.rows(isis) = in_circles_is;

  arma::umat all_circles = arma::join_cols(two, two);

  areas(find(ones == 1)) = pow(r, 2) * datum::pi;
  areas(itwos) = atwos;

  // Work out areas of relationships of 3 or more sets.
  for (arma::uword i = n * (n - 1) / 2 + n; i < id.n_rows; i++) {
    arma::uvec curr_set = arma::find(id.row(i) == 1);
    arma::uvec twoway = locate(two_a, curr_set) && locate(two_b, curr_set);

    arma::uvec twotwo = arma::join_cols(twoway, twoway);
    arma::uvec in_all = arma::sum(in_circles.cols(curr_set), 1) == curr_set.n_elem;

    in_all = in_all && twotwo;

    arma::uvec veci = arma::find(in_all == 1);

    arma::umat circles = all_circles.rows(veci);

    arma::uword crow = circles.n_rows;

    if (crow < 2) {
      arma::uword l = curr_set(r(curr_set).index_min());
      arma::uvec la(1);
      la.fill(l);

      arma::uvec ff = locate(two_a, la) || locate(two_b, la);

      if (all(contained(find(ff == 1)))) {
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
      areas(i) = polyarc_areas(curr_intpoints.col(0), curr_intpoints.col(1),
                               r, circles);
    }
  }
  return arma::conv_to< std::vector<double> >::from(areas);;
}

// [[Rcpp::export]]
double stress(arma::vec areas, arma::vec fit) {
  double sst = arma::accu(pow(fit, 2));
  double slope = arma::accu(areas % fit) / arma::accu(pow(areas, 2));
  double sse = arma::accu(pow(fit - areas * slope, 2));
  return sse / sst;
}

// [[Rcpp::export]]
double compute_fit(arma::vec par, arma::vec areas, arma::umat id,
                   arma::umat two, arma::uvec twos, arma::uvec ones,
                   arma::uword cost) {
  arma::vec fit = return_intersections(par, areas, id, two, twos, ones);

  switch(cost) {
  case 0:
    // venneuler stress function
    return arma::accu(pow(areas - fit, 2) / (fit + 1e-6));
  default:
    // eulerAPE cost function
    return stress(areas, fit);
  }
}