// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// #define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "ellipse_conversions.h"
#include "solvers.h"
#include "helpers.h"

using namespace Rcpp;
using namespace arma;

arma::mat intersect_conic_line(
    const arma::mat A,
    const arma::vec l
  ) {
  mat::fixed<3, 3> B, M;
  mat::fixed<3, 2> out;

  M = skewsymmat(l);
  B = M.t() * A * M;

  // Pick a non-zero element of l
  uword i = as_scalar(find(l != 0, 1));
  uvec li = {0, 1, 2};
  li.shed_row(i);

  double alpha = (1.0/l(i))*sqrt(-det(symmatl(B.submat(li, li))));

  if (is_finite(alpha)) {
    mat C = B + alpha*M;
    uvec ind = ind2sub(size(C), find(C != 0, 1));
    uword i0 = ind(0);
    uword i1 = ind(1);

    out.col(0) = C.row(i0).t() / C(i0, 2);
    out.col(1) = C.col(i1)     / C(2, i1);
  } else {
    out.fill(datum::nan);
  }

  return out;
}

void split_conic(const arma::mat A, arma::vec& g, arma::vec& h) {
  mat::fixed<3, 3> B = -adjoint(A);

  // Find non-zero index on the diagonal
  uvec i = find(B.diag() != 0, 1);

  if (i.n_elem == 1) {
    uword ii = as_scalar(i);
    std::complex<double> Bii = sqrt(B(ii, ii));

    if (std::real(Bii) >= 0) {
      cx_vec p = B.col(ii) / Bii;
      cx_mat C = A + skewsymmat(p);
      uvec ij = ind2sub(size(C), find(C != 0, 1));

      if (ij.n_elem > 0) {
        // Extract the lines
        g = real(C.row(ij(0)).t());
        h = real(C.col(ij(1)));
      }
    }
  }
}

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

  // Select the largest real root
  double lambda = 0;
  for (auto root : roots)
    if (std::imag(root) == 0)
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);

  mat::fixed<3, 3> C = lambda*A + B;

  C(find(abs(C) < sqrt(datum::eps))).zeros();

  // Split the degenerate conic into lines g and h
  vec::fixed<3> g, h;
  g.fill(datum::nan);
  h.fill(datum::nan);
  split_conic(C, g, h);

  // Intersect one of the conics with each line to get points p q
  mat::fixed<3, 4> out;
  out.cols(0, 1) = intersect_conic_line(A, g);
  out.cols(2, 3) = intersect_conic_line(A, h);

  return out;
}

arma::umat adopt(
    arma::mat points,
    arma::mat ellipses,
    arma::uword n,
    arma::uword i,
    arma::uword j
  ) {
  umat out(n, 4);

  for (uword l = 0; l < n; l++) {
    if (l == i) {
      out.row(i).ones();
    } else if (l == j) {
      out.row(j).ones();
    } else {
      // Check if the points lie inside the ellipse
      rowvec x   = points.row(0);
      rowvec y   = points.row(1);
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

double ellipse_segment(arma::vec v, arma::vec p0, arma::vec p1) {
  vec    hk  = v.subvec(0, 1);
  double a   = v(2);
  double b   = v(3);
  double phi = v(4);
  arma::vec::fixed<2> x, y, sector, theta;

  p0 = rotate(-phi) * translate(-hk) * p0;
  p1 = rotate(-phi) * translate(-hk) * p1;

  x(0) = p0(0);
  x(1) = p1(0);
  y(0) = p0(1);
  y(1) = p1(1);

  // Find the angle to the points from the center of the ellipse.
  theta = arma::atan2(y, x);

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
  uvec ind = sort_index(atan2(x_int - accu(x_int)/n, y_int - accu(y_int)/n));

  // Reorder vectors and matrix based on angles to centroid
  points  = points.cols(ind);
  parents = parents.cols(ind);
  x_int = x_int(ind);
  y_int = y_int(ind);
  double area = 0;

  for (uword i = 0, j = n - 1; i < n; i++) {
    // First discover which ellipses the points belong to
    uvec ii = set_intersect(parents.col(i).t(), parents.col(j).t());
    uword i_n = ii.n_elem;
    vec areas(i_n);

    // Ellipse segment
    for (uword k = 0; k < i_n; k++) {
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

double disjoint_or_subset(arma::mat M) {
  rowvec areas = M.row(2) % M.row(3) * datum::pi;
  uword i = areas.index_min();
  double x = M(0, i);
  double y = M(1, i);
  M.shed_col(i);

  rowvec xmh    = x - M.row(0);
  rowvec ymk    = y - M.row(1);
  rowvec a2     = pow(M.row(2), 2);
  rowvec b2     = pow(M.row(3), 2);
  rowvec phi    = M.row(4);
  rowvec cosphi = cos(phi);
  rowvec sinphi = sin(phi);

  urowvec is_subset = pow(xmh%cosphi + ymk%sinphi, 2)/a2 +
                      pow(xmh%sinphi - ymk%cosphi, 2)/b2 < 1;

  return all(is_subset) ? areas(i) : 0;
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
    conics.slice(i) = standard_to_matrix(ellipses.col(i));

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
    } else {
      // Two or more sets
      uvec subparents(parents.n_cols);
      for (uword q = 0; q < parents.n_cols; q++)
        subparents(q) = any(parents(0, q) == ids) * any(parents(1, q) == ids);

      uvec int_points =
        find((sum(adopters.rows(ids)).t() == ids.n_elem)%subparents);

      if (int_points.n_elem == 0) {
        // No intersections: either disjoint or subset
        areas(i) = disjoint_or_subset(ellipses.cols(ids));

      } else {
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points));
      }
    }
  }

  vec out(n_combos, fill::zeros);

  for (uword i = n_combos; i-- > 0;) {
    umat subareas = id.cols(find(id.row(i) == 1));
    out(i) = areas(i) - accu(out(find(sum(subareas, 1) == subareas.n_cols)));
  }

  return clamp(out, 0, datum::inf);
}
