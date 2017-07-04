// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "ellipse_conversions.h"
#include "solvers.h"
#include "helpers.h"
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat intersect_conic_line(
    const arma::mat A,
    const arma::vec l
  ) {
  mat::fixed<3, 3> B, M;
  mat::fixed<3, 3> C;
  mat::fixed<2, 2> Bb;
  mat::fixed<3, 2> out;
  double alpha;

  M = skewsymmat(l);
  B = M.t() * A * M;

  // Pick a non-zero element of l
  uword i = as_scalar(find(l != 0, 1));
  uvec li = {0, 1, 2};
  li.shed_row(i);

  alpha = (1.0/l(i))*sqrt(-det(symmatl(B.submat(li, li))));

  if (is_finite(alpha)) {
    C = B + alpha*M;
    uvec ind = ind2sub(size(C), find(C != 0, 1));
    out.col(0) = C.row(ind(0)).t() / C(ind(0), 2);
    out.col(1) = C.col(ind(1)) / C(2, ind(1));
  } else {
    out.fill(datum::nan);
  }

  return out;
}

// [[Rcpp::export]]
arma::mat split_conic(const arma::mat A) {
  mat::fixed<3, 3> B;
  mat::fixed<3, 2> out;
  cx_mat::fixed<3, 3> C;

  B = -adjoint(A);

  // Find non-zero index on the diagonal
  uvec i = find(B.diag() != 0, 1);

  if (i.n_elem == 1) {
    uword ii = i(0);
    std::complex<double> Bii = sqrt(B(ii, ii));

    if (std::real(Bii) >= 0) {
      cx_vec p = B.col(ii) / Bii;
      C = A + skewsymmat(p);

      uvec ij = ind2sub(size(C), find(C != 0, 1));
      if (ij.n_elem > 0) {
        // Extract the lines
        out.col(0) = real(C.row(ij(0)).t());
        out.col(1) = real(C.col(ij(1)));
      } else {
        out.fill(datum::nan);
      }
    } else {
      out.fill(datum::nan);
    }
  } else {
    out.fill(datum::nan);
  }
  return out;
}

// [[Rcpp::export]]
arma::mat intersect_conics(const arma::mat A, const arma::mat B) {
  vec::fixed<4> v;

  v(0) = det(A);
  v(1) = det(join_rows(A.cols(0, 1), B.col(2))) +
    det(join_rows(join_rows(A.col(0), B.col(1)), A.col(2))) +
    det(join_rows(B.col(0), A.cols(1, 2)));
  v(2) = det(join_rows(A.col(0), B.cols(1, 2))) +
    det(join_rows(join_rows(B.col(0), A.col(1)), B.col(2))) +
    det(join_rows(B.cols(0, 1), A.col(2)));
  v(3) = det(B);

  // Find the cubic roots
  cx_vec::fixed<3> roots = solve_cubic(v);

  // Select a real root
  double lambda = 0;
  for (auto root : roots)
    if (std::imag(root) == 0)
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);

  mat::fixed<3, 3> C = lambda*A + B;

  C.transform([](double x) {return (std::abs(x) < sqrt(datum::eps) ? 0 : x);});

  // Split the degenerate conic into lines g and h
  mat::fixed<3, 2> lines = split_conic(C);

  // Intersect one of the conics with each line to get points p q
  mat::fixed<3, 4> out;
  out.cols(0, 1) = intersect_conic_line(A, lines.col(0));
  out.cols(2, 3) = intersect_conic_line(A, lines.col(1));

  return out;
}

// [[Rcpp::export]]
arma::umat adopt(
    arma::mat points,
    arma::mat ellipses,
    arma::uword n,
    arma::uword i,
    arma::uword j
  ) {
  umat out(n, 4, fill::zeros);

  for (uword l = 0; l < n; l++) {
    //mat pp = translate(hk) * rotate(-ellipses(4, l)) * translate(-hk) * points;

    if (l == i) {
      out.row(i).ones();
    } else if (l == j) {
      out.row(j).ones();
    } else {
      // Check if the points lie inside the ellipse
      rowvec x = points.row(0);
      rowvec y = points.row(1);
      double h   = ellipses(0, l);
      double k   = ellipses(1, l);
      double a   = ellipses(2, l);
      double b   = ellipses(3, l);
      double phi = ellipses(4, l);
      out.row(l) = pow((x - h)*cos(phi) + (y - k)*sin(phi), 2)/pow(a, 2) +
                   pow((x - h)*sin(phi) - (y - k)*cos(phi), 2)/pow(b, 2) < 1;
    }
  }
  return out;
}

double ellipse_area(arma::vec v) {
  return datum::pi * v(2) * v(3);
}

// [[Rcpp::export]]
double ellipse_segment(arma::vec v, arma::vec p0, arma::vec p1) {
  vec    hk  = v.subvec(0, 1);
  double a   = v[2];
  double b   = v[3];
  double phi = v[4];
  arma::vec::fixed<2> x, y, sector, theta;

  p0 = rotate(-phi) * translate(-hk) * p0;
  p1 = rotate(-phi) * translate(-hk) * p1;

  x[0] = p0[0];
  x[1] = p1[0];
  y[0] = p0[1];
  y[1] = p1[1];

  // Find the angle to the points from the center of the ellipse.
  theta = arma::atan2(y, x);

  // Add 2 pi to negative radians
  //theta.transform([](double x) {return (x >= 0 ? x : (2*datum::pi + x));});

  if (theta(1) < theta(0))
    theta(1) += 2*datum::pi;

  // Triangle part of the sector
  double triangle = 0.5*std::abs(x(1)*y(0) - x(0)*y(1));

  double dtheta = theta(1) - theta(0);

  if (dtheta <= datum::pi) {
    sector = 0.5*a*b*(theta - atan2((b - a)*sin(2*theta),
                                    (b + a + (b - a)*cos(2*theta))));
    return sector(1) - sector(0) - triangle;
  } else {
    theta(0) += 2*datum::pi;
    sector = 0.5*a*b*(theta - atan2((b - a)*sin(2*theta),
                                    (b + a + (b - a)*cos(2*theta))));
    return a*b*datum::pi - sector(0) + sector(1) + triangle;
  }
}

double polysegments(
    arma::mat points,
    arma::mat ellipses,
    arma::umat parents
  ) {
  vec x_int = points.row(0).t();
  vec y_int = points.row(1).t();
  uword n = points.n_cols;

  // Sort points by their angle to the centroid
  vec angle = atan2(x_int - accu(x_int)/n, y_int - accu(y_int)/n);
  //angle.transform([](double x) {return (x >= 0 ? x : (2*datum::pi + x));});
  uvec ind = sort_index(angle);

  // Reorder vectors and matrix based on angles to centroid
  points  = points.cols(ind);
  parents = parents.cols(ind);
  x_int = x_int(ind);
  y_int = y_int(ind);
  double area = 0;

  for (uword i = 0, j = n - 1; i < n; i++) {
    // First discover which ellipses the points belong to
    uvec ii = set_intersect(parents.col(i).t(),
                            parents.col(j).t());
    vec areas(ii.n_elem);

    // Ellipse segment
    for (uword k = 0; k < ii.n_elem; k++) {
      areas(k) = ellipse_segment(ellipses.col(ii(k)),
                                 points.col(i),
                                 points.col(j));
    }

    // If we have two circles at these points, pick the smaller
    area += areas.min();

    // Triangular segment
    area += ((x_int(j) + x_int(i)) * (y_int(j) - y_int(i))) / 2;
    j = i;
  }
  return area;
}

// [[Rcpp::export]]
arma::mat intersect_ellipses(const arma::vec par) {
  uword n = par.n_elem/5;
  uword nint = 4 * n * (n - 1) / 2;
  umat  id = bit_index(n);
  uword n_combos = id.n_rows;
  vec   areas(n_combos, fill::zeros);

  // Set up a matrix of the ellipses in standard form and a cube of conics
  mat  ellipses = reshape(par, 5, n);
  cube conics(3, 3, n);
  for (uword i = 0; i < n; i++)
    conics.slice(i) = standard_to_matrix2(ellipses.col(i));

  // Collect all points of intersection
  mat  points   (3, nint);
  umat parents  (2, nint);
  umat adopters (n, nint);

  for (uword i = 0, k = 0; i < n - 1; i++) {
    for (uword j = i + 1; j < n; j++) {
      points.cols(k, k + 3) = intersect_conics(conics.slice(i),
                                               conics.slice(j));
      adopters.cols(k, k + 3) = adopt(points.cols(k, k + 3), ellipses, n, i, j);
      parents(0, span(k, k + 3)).fill(i);
      parents(1, span(k, k + 3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  uvec not_na = find_finite(points.row(0));
  points   = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents  = parents.cols(not_na);


  // Loop over each set combination
  for (uword i = 0; i < n_combos; i++) {
    urowvec sets = id.row(i);
    uvec ids = find(sets == 1);

    if (ids.n_elem == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.col(as_scalar(ids)));
    } else if (ids.n_elem > 1) {
      // Two or more sets
      uvec subparents(parents.n_cols);
      for (uword q = 0; q < parents.n_cols; q++) {
        subparents(q) = any(parents(0, q) == ids) * any(parents(1, q) == ids);
      }

      uvec int_points = find((sum(adopters.rows(ids)).t() == ids.n_elem)%subparents);
      uword n_points = int_points.n_elem;

      if (n_points == 0) {
        // No intersections; either disjoint or subset
        mat curr = ellipses.cols(ids);
        rowvec A = curr.row(2) % curr.row(3) * datum::pi;
        double x = curr(0, A.index_min());
        double y = curr(1, A.index_min());
        curr.shed_col(A.index_min());

        rowvec h   = curr.row(0);
        rowvec k   = curr.row(1);
        rowvec a   = curr.row(2);
        rowvec b   = curr.row(3);
        rowvec phi = curr.row(4);
        urowvec is_subset =
          pow((x - h)%cos(phi) + (y - k)%sin(phi), 2)/pow(a, 2) +
          pow((x - h)%sin(phi) - (y - k)%cos(phi), 2)/pow(b, 2) < 1;
        if (all(is_subset)) {
          areas(i) = min(A);
        } else {
          areas(i) = 0;
        }
      } else {
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points));
      }
    }
  }

  vec areas_out(n_combos, fill::zeros);

  for (uword i = n_combos; i --> 0; ) {
    umat subareas = id.cols(find(id.row(i) == 1));
    uvec prev_areas = find(sum(subareas, 1) == subareas.n_cols);
    areas_out(i) = areas(i) - accu(areas_out(prev_areas));
  }
  areas.print();
  return areas_out;

  //return clamp(areas_out, 0, datum::inf);
}

