#ifndef eulerr_intersections_
#define eulerr_intersections_

#include <RcppArmadillo.h>
#include "solver.h"
#include "constants.h"
#include "transformations.h"

// Split a degenerate conic into two lines
inline
void
split_conic(const arma::mat& A, arma::vec& g, arma::vec& h) {
  arma::mat::fixed<3, 3> B = -adjoint(A);

  // Find non-zero index on the diagonal
  arma::uword i = arma::index_max(arma::abs(B.diag()));
  std::complex<double> Bii = std::sqrt(B(i, i));

  if (std::real(Bii) >= 0.0) {
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
inline
void
intersect_conic_line(const arma::mat& A,
                     arma::vec& l,
                     arma::subview<double>&& points) {
  arma::mat::fixed<3, 3> M = skewsymmat(l);
  arma::mat::fixed<3, 3> B = M.t()*A*M;
  arma::vec::fixed<3> l_abs = arma::abs(l);

  l(arma::find(l_abs < small)).zeros();
  // Pick a non-zero element of l
  if (arma::any(l_abs > 0.0)) {
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

      points.unsafe_col(0) = C.row(i0).t() / C(i0, 2);
      points.unsafe_col(1) = C.col(i1)     / C(2, i1);
    } else {
      points.fill(arma::datum::nan);
    }
  } else {
    points.fill(arma::datum::nan);
  }
}

// Intersect two conics, returning 0-4 intersection points
inline
void
intersect_conics(const arma::mat& A,
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
  double lambda = 0.0;
  for (auto root : roots)
    if (std::imag(root) == 0.0)
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

#endif
