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
  arma::mat::fixed<3, 3> B, M;
  arma::mat::fixed<2, 2> Bb;
  arma::mat::fixed<3, 2> out;
  double alpha;

  M = skewsymmat(l);
  B = M.t()*A*M;

  // Pick a non-zero element of l
  arma::uword i = arma::as_scalar(arma::find(l != 0, 1));
  arma::uvec li = {0, 1, 2};
  li.shed_row(i);

  alpha = sqrt(-arma::det(arma::symmatl(B.submat(li, li))))/l(i);

  if (arma::is_finite(alpha)) {
    B += alpha*M;
    arma::uvec ind = arma::ind2sub(size(B), arma::find(B != 0, 1));
    out.col(0) = B.row(ind(0)).t() / B(ind(0), 2);
    out.col(1) = B.col(ind(1))     / B(2, ind(1));
  } else {
    out.fill(datum::nan);
  }

  return out;
}

// [[Rcpp::export]]
arma::mat split_conic(const arma::mat A) {
  arma::mat::fixed<3, 3> B;
  arma::mat::fixed<3, 2> out;
  arma::cx_mat::fixed<3, 3> C;
  arma::cx_vec::fixed<3> p;

  B = -adjoint(A);

  // Find non-zero index on the diagonal
  arma::uvec i = arma::find(B.diag() != 0, 1);

  if (i.n_elem > 0) {
    arma::uword ii = i(0);
    std::complex<double> Bii = sqrt(B(ii, ii));

    if (std::real(Bii) >= 0) {
      p = B.col(ii) / Bii;
      C = A + skewsymmat(p);

      arma::uvec ij = arma::ind2sub(size(C), arma::find(C != 0, 1));
      if (ij.n_elem > 0) {
        // Extract the lines
        out.col(0) = arma::real(C.row(ij(0)).t());
        out.col(1) = arma::real(C.col(ij(1)));
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
  arma::vec::fixed<4> v;
  arma::mat::fixed<3, 3> C;
  arma::mat::fixed<3, 2> lines;
  arma::cx_vec::fixed<3> roots;
  arma::mat::fixed<3, 4> out;

  v(0) = arma::det(A);
  v(1) = arma::det(arma::join_rows(A.cols(0, 1), B.col(2))) +
    det(arma::join_rows(arma::join_rows(A.col(0), B.col(1)), A.col(2))) +
    det(arma::join_rows(B.col(0), A.cols(1, 2)));
  v(2) = arma::det(arma::join_rows(A.col(0), B.cols(1, 2))) +
    det(arma::join_rows(arma::join_rows(B.col(0), A.col(1)), B.col(2))) +
    det(arma::join_rows(B.cols(0, 1), A.col(2)));
  v(3) = arma::det(B);

  // Find the cubic roots
  roots = solve_cubic(v);

  // Select a real root
  double lambda = 0;
  for (auto root : roots)
    if (std::imag(root) == 0)
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);

  C = lambda*A + B;

  //C.transform([](double x) {return (std::abs(x) < sqrt(datum::eps) ? 0 : x);});
  C(find(arma::abs(C) < sqrt(datum::eps))).zeros();

  // Split the degenerate conic into lines g and h
  lines = split_conic(C);

  // Intersect one of the conics with each line to get points p q
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
  arma::umat out(n, 4);

  arma::rowvec x = points.row(0);
  arma::rowvec y = points.row(1);

  for (uword l = 0; l < n; l++) {
    if (l == i) {
      out.row(i).ones();
    } else if (l == j) {
      out.row(j).ones();
    } else {
      // Check if the points lie inside the ellipse
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
  arma::vec hk = v.subvec(0, 1);
  double a     = v[2];
  double b     = v[3];
  double phi   = v[4];
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
    return a*b*datum::pi - (sector(0) - sector(1) - triangle);
  }
}

double polysegments(
    arma::mat points,
    arma::mat ellipses,
    arma::umat parents
  ) {
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
  double area = 0;

  for (arma::uword i = 0, j = n - 1; i < n; i++) {
    // First discover which ellipses the points belong to
    arma::uvec ii = set_intersect(parents.col(i).t(),
                                  parents.col(j).t());
    arma::vec areas(ii.n_elem);

    // Ellipse segment
    for (arma::uword k = 0; k < ii.n_elem; k++) {
      areas(k) = ellipse_segment(ellipses.col(ii(k)),
                                 points.col(i),
                                 points.col(j));
    }

    // If we have two circles at these points, pick the smaller
    area += areas.min();

    // Triangular segment
    area += ((x_int(j) + x_int(i))*(y_int(j) - y_int(i)))/2;
    j = i;
  }
  return area;
}


double disjoint_or_subset(arma::mat m) {
  arma::rowvec h, k, a, b, phi;
  arma::rowvec A = m.row(2) % m.row(3) * datum::pi;
  arma::uword i = A.index_min();
  double x = m(0, i);
  double y = m(1, i);
  m.shed_col(i);

  h   = m.row(0);
  k   = m.row(1);
  a   = m.row(2);
  b   = m.row(3);
  phi = m.row(4);

  arma::urowvec is_subset =
    arma::pow((x - h)%arma::cos(phi) + (y - k)%arma::sin(phi), 2)/(a*a) +
    arma::pow((x - h)%arma::sin(phi) - (y - k)%arma::cos(phi), 2)/(b*b) < 1;

  return all(is_subset) ? A.min() : 0;
}


// [[Rcpp::export]]
arma::mat intersect_ellipses(const arma::vec par) {
  arma::uword n = par.n_elem/5;
  arma::uword nint = 4*n*(n - 1)/2;
  arma::umat  id = bit_index(n);
  arma::uword n_combos = id.n_rows;
  arma::vec   areas(n_combos, fill::zeros);
  arma::mat   ellipses = reshape(par, 5, n);
  arma::cube  conics(3, 3, n);
  arma::mat   points   (3, nint);
  arma::umat  parents  (2, nint);
  arma::umat  adopters (n, nint);

  // Set up a matrix of the ellipses in standard form and a cube of conics

  for (arma::uword i = 0; i < n; i++)
    conics.slice(i) = standard_to_matrix2(ellipses.col(i));

  // Collect all points of intersection
  for (arma::uword i = 0, k = 0; i < n - 1; i++) {
    for (arma::uword j = i + 1; j < n; j++) {
      points.cols(k, k + 3) = intersect_conics(conics.slice(i),
                                               conics.slice(j));
      adopters.cols(k, k + 3) = adopt(points.cols(k, k + 3), ellipses, n, i, j);
      parents(0, span(k, k + 3)).fill(i);
      parents(1, span(k, k + 3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  arma::uvec not_na = arma::find_finite(points.row(0));
  points   = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents  = parents.cols(not_na);


  // Loop over each set combination
  for (arma::uword i = 0; i < n_combos; i++) {
    arma::urowvec sets = id.row(i);
    arma::uvec ids = find(sets == 1);

    if (ids.n_elem == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.col(as_scalar(ids)));
    } else {
      // Two or more sets
      arma::uvec subparents(parents.n_cols);
      for (arma::uword q = 0; q < parents.n_cols; q++) {
        subparents(q) = arma::any(parents(0, q) == ids)*
                        arma::any(parents(1, q) == ids);
      }

      arma::uvec int_points =
        arma::find((arma::sum(adopters.rows(ids)).t() == ids.n_elem)%subparents);

      if (int_points.n_elem == 0) {
        // No intersections; either disjoint or subset
        mat curr = ellipses.cols(ids);
        areas(i) = disjoint_or_subset(curr);

      } else {
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points));
      }
    }
  }

  arma::vec areas_out(n_combos, fill::zeros);

  for (arma::uword i = n_combos; i > 0; i--) {
    arma::umat subareas = id.cols(arma::find(id.row(i) == 1));
    areas_out(i) = areas(i) - arma::accu(
      areas_out(arma::find(arma::sum(subareas, 1) == subareas.n_cols))
    );
  }
  areas.print();
  return areas_out;

  //return clamp(areas_out, 0, datum::inf);
}

