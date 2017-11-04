// #define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "transformations.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"

// Split a degenerate conic into two lines
void split_conic(const arma::mat& A, arma::vec& g, arma::vec& h) {
  arma::mat::fixed<3, 3> B = -adjoint(A);

  // Find non-zero index on the diagonal
  arma::uword i = arma::index_max(arma::abs(B.diag()));
  std::complex<double> Bii = std::sqrt(B(i, i));

  if (std::real(Bii) >= 0) {
    arma::cx_mat::fixed<3, 3> C = A + skewsymmat(B.col(i)/Bii);
    // Extract the lines
    arma::uvec ij =
      arma::ind2sub(arma::size(3, 3),
                    arma::index_max(arma::abs(arma::vectorise(C))));
    g = arma::real(C.row(ij(0)).t());
    h = arma::real(C.col(ij(1)));
  } else {
    g.fill(arma::datum::nan);
    h.fill(arma::datum::nan);
  }
}

// Intersect a conic with two lines to return 0 to 4 intersection points
void intersect_conic_line(const arma::mat& A,
                          arma::vec& l,
                          arma::subview<double>&& points) {
  arma::mat::fixed<3, 3> M = skewsymmat(l);
  arma::mat::fixed<3, 3> B = M.t() * A * M;
  arma::vec::fixed<3> l_abs = arma::abs(l);

  l(arma::find(l_abs < small)).zeros();
  // Pick a non-zero element of l
  if (arma::any(l_abs > 0)) {
    arma::uword i = arma::index_max(l_abs);
    arma::uvec li = arma::linspace<arma::uvec>(0, 2, 3);
    li.shed_row(i);

    double alpha = std::sqrt(-arma::det(arma::symmatl(B.submat(li, li))))/l(i);

    arma::mat::fixed<3, 3> C = B + alpha*M;

    arma::vec::fixed<9> C_abs = arma::abs(arma::vectorise(C));

    if (arma::any(C_abs > small)) {
      arma::uvec ind = arma::ind2sub(arma::size(3, 3), arma::index_max(C_abs));
      arma::uword i0 = ind(0);
      arma::uword i1 = ind(1);

      points.col(0) = C.row(i0).t() / C(i0, 2);
      points.col(1) = C.col(i1)     / C(2, i1);
    } else {
      points.fill(arma::datum::nan);
    }
  } else {
    points.fill(arma::datum::nan);
  }
}

// Intersect two conics, returning 0-4 intersection points
void intersect_conics(const arma::mat& A,
                      const arma::mat& B,
                      arma::subview<double>&& points) {
  double alpha = arma::det(A);
  double beta = arma::det(arma::join_rows(A.cols(0, 1), B.col(2))) +
    arma::det(arma::join_rows(arma::join_rows(A.col(0), B.col(1)), A.col(2))) +
    arma::det(arma::join_rows(B.col(0), A.cols(1, 2)));
  double gamma = arma::det(arma::join_rows(A.col(0), B.cols(1, 2))) +
    arma::det(arma::join_rows(arma::join_rows(B.col(0), A.col(1)), B.col(2))) +
    arma::det(arma::join_rows(B.cols(0, 1), A.col(2)));
  double delta = arma::det(B);

  // Find the cubic roots
  arma::cx_vec::fixed<3> roots = solve_cubic(alpha, beta, gamma, delta);

  // Select the largest real root
  double lambda = 0;
  for (auto root : roots)
    if (std::imag(root) == 0)
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);

  arma::mat::fixed<3, 3> C = lambda*A + B;

  C(arma::find(arma::abs(C) < small)).zeros();

  // Split the degenerate conic into lines g and h
  arma::vec::fixed<3> g, h;
  split_conic(C, g, h);

  // Intersect one of the conics with each line to get 0 to 4 points
  if (arma::is_finite(g) && arma::is_finite(h)) {
    intersect_conic_line(A, g, points.cols(0, 1));
    intersect_conic_line(A, h, points.cols(2, 3));
  } else {
    points.fill(arma::datum::nan);
  }
}

// See which ellipses contain a given set of points
void adopt(const arma::mat& points,
           const arma::mat& ellipses,
           const arma::uword n,
           const arma::uword i,
           const arma::uword j,
           arma::subview<arma::uword>&& out) {
  for (arma::uword l = 0; l < n; ++l) {
    if ((l == i) | (l == j)) {
      out.row(l).ones();
    } else {
      arma::rowvec x = points.row(0);
      arma::rowvec y = points.row(1);
      double h = ellipses(0, l);
      double k = ellipses(1, l);
      double phi = ellipses(4, l);

      // Check if the points lie inside the ellipse
      out.row(l) = arma::pow((x - h)*std::cos(phi) + (y - k)*std::sin(phi), 2)/
        std::pow(ellipses(2, l), 2) +
        arma::pow((x - h)*std::sin(phi) - (y - k)*std::cos(phi), 2)/
        std::pow(ellipses(3, l), 2) < 1;
    }
  }
}

// See if a group of ellipses are completely disjoint or a russian doll
double disjoint_or_subset(arma::mat M) {
  arma::rowvec areas = M.row(2)%M.row(3)*arma::datum::pi;
  arma::uword i = areas.index_min();
  double x = M(0, i);
  double y = M(1, i);
  M.shed_col(i);

  arma::rowvec xmh    = x - M.row(0);
  arma::rowvec ymk    = y - M.row(1);
  arma::rowvec phi    = M.row(4);
  arma::rowvec cosphi = arma::cos(phi);
  arma::rowvec sinphi = arma::sin(phi);

  arma::urowvec is_subset =
    arma::pow(xmh%cosphi + ymk%sinphi, 2)/arma::pow(M.row(2), 2) +
    arma::pow(xmh%sinphi - ymk%cosphi, 2)/arma::pow(M.row(3), 2) < 1.0;

  return arma::all(is_subset) ? areas(i) : 0.0;
}

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec intersect_ellipses(const arma::vec& par,
                             const bool circles) {
  arma::uword n_pars   = circles ? 3 : 5;
  arma::uword n        = par.n_elem/n_pars;
  arma::uword n_int    = 2*n*(n - 1);
  arma::umat  id       = bit_index(n);
  arma::uword n_combos = id.n_rows;

  // Set up a matrix of the ellipses in standard form and a cube of conics
  arma::mat ellipses = arma::reshape(par, n_pars, n);

  if (circles) {
    ellipses.insert_rows(3, ellipses.row(2));
    ellipses.insert_rows(4, 1);
  }

  arma::cube conics(3, 3, n);

  for (arma::uword i = 0; i < n; ++i) {
    conics.slice(i) = standard_to_matrix(ellipses.col(i));
  }

  // Collect all points of intersection
  arma::mat  points   (3, n_int);
  arma::umat parents  (2, n_int);
  arma::umat adopters (n, n_int);

  for (arma::uword i = 0, k = 0; i < n - 1; ++i) {
    for (arma::uword j = i + 1; j < n; ++j) {
      intersect_conics(conics.slice(i), conics.slice(j), points.cols(k, k + 3));
      adopt(points.cols(k, k + 3), ellipses, n, i, j, adopters.cols(k, k + 3));
      parents(0, arma::span(k, k + 3)).fill(i);
      parents(1, arma::span(k, k + 3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  arma::uvec not_na = arma::find_finite(points.row(0));
  points   = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents  = parents.cols(not_na);
  arma::uvec owners(parents.n_cols);

  // Loop over each set combination
  arma::vec areas(n_combos);

  for (arma::uword i = 0; i < n_combos; ++i) {
    arma::uvec ids = arma::find(id.row(i) == 1);
    arma::uword n_ids = ids.n_elem;

    if (n_ids == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.col(arma::as_scalar(ids)));
    } else {
      // Two or more sets
      for (arma::uword q = 0; q < parents.n_cols; ++q) {
        owners(q) = set_intersect(parents.col(q), ids).n_elem == 2;
      }

      arma::uvec int_points =
        arma::find((arma::sum(adopters.rows(ids)).t() == n_ids)%owners);

      if (int_points.n_elem == 0) {
        // No intersections: either disjoint or subset
        areas(i) = disjoint_or_subset(ellipses.cols(ids));
      } else {
        // Compute the area of the overlap
        bool failure = false;
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points),
                                failure);
        if (failure) {
          // Resort to approximation if exact calculation fails
          // TODO: Use a better fallback approximation
          areas(i) = montecarlo(ellipses.cols(ids));
        }
      }
    }
  }

  arma::vec out(n_combos, arma::fill::zeros);

  for (arma::uword i = n_combos; i-- > 0;) {
    arma::umat subareas = id.cols(arma::find(id.row(i) == 1));
    out(i) = areas(i) -
      arma::accu(out(arma::find(arma::sum(subareas, 1) == subareas.n_cols)));
  }

  return arma::clamp(out, 0, out.max());
}

// Compute the sums of squares between the actual and desired areas
// [[Rcpp::export]]
double optim_final_loss(const arma::vec& par,
                        const arma::vec& areas,
                        const bool circles) {
  return arma::accu(arma::square(areas - intersect_ellipses(par, circles)));
}
