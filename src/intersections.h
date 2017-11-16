#ifndef eulerr_intersections_
#define eulerr_intersections_

#include <RcppArmadillo.h>
#include "solver.h"
#include "constants.h"
#include "transformations.h"

using namespace arma;

// Split a degenerate conic into two lines
inline
arma::mat
split_conic(const mat& A) {
  mat::fixed<3, 3> B = -adjoint(A);

  // Find non-zero index on the diagonal
  uword i = index_max(abs(B.diag()));
  std::complex<double> Bii = std::sqrt(B(i, i));

  mat::fixed<3, 2> out;

  if (std::real(Bii) >= 0.0) {
    cx_mat::fixed<3, 3> C = A + skewsymmat(B.col(i)/Bii);
    // Extract the lines
    uvec ij = ind2sub(size(3, 3), index_max(abs(vectorise(C))));
    out.col(0) = real(C.row(ij(0)).t());
    out.col(1) = real(C.col(ij(1)));
  } else {
    out.fill(datum::nan);
  }
  return out;
}

// Intersect a conic with two lines to return 0 to 4 intersection points
inline
arma::mat
intersect_conic_line(const arma::mat& A, const arma::vec& l) {
  mat::fixed<3, 3> M = skewsymmat(l);
  mat::fixed<3, 3> B = M.t()*A*M;
  vec::fixed<3> l_abs = abs(l);
  mat::fixed<3, 2> out;

  // Pick a non-zero element of l
  if (any(l_abs > small)) {
    uword i = index_max(l_abs);
    uvec li = regspace<uvec>(0, 2);
    li.shed_row(i);

    double alpha = std::sqrt(-det(symmatl(B.submat(li, li))))/l(i);

    mat::fixed<3, 3> C = B + alpha*M;

    vec::fixed<9> C_abs = abs(vectorise(C));

    if (any(C_abs > small)) {
      uvec ind = ind2sub(size(3, 3), index_max(C_abs));
      uword i0 = ind(0);
      uword i1 = ind(1);

      out.col(0) = C.row(i0).t() / C(i0, 2);
      out.col(1) = C.col(i1)     / C(2, i1);
    } else {
      out.fill(datum::nan);
    }
  } else {
    out.fill(datum::nan);
  }
  return out;
}

// Intersect two conics, returning 0-4 intersection points
inline
arma::mat
intersect_conics(const arma::mat& A,
                 const arma::mat& B) {
  double alpha = det(A);
  double beta = det(join_rows(A.cols(0, 1), B.col(2))) +
                det(join_rows(join_rows(A.col(0), B.col(1)), A.col(2))) +
                det(join_rows(B.col(0), A.cols(1, 2)));
  double gamma = det(join_rows(A.col(0), B.cols(1, 2))) +
                 det(join_rows(join_rows(B.col(0), A.col(1)), B.col(2))) +
                 det(join_rows(B.cols(0, 1), A.col(2)));
  double delta = det(B);

  // Find the cubic roots
  cx_vec::fixed<3> roots = solve_cubic(alpha, beta, gamma, delta);

  // Select the largest real root
  double lambda = 0.0;
  for (auto root : roots)
    if (std::imag(root) == 0.0)
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);

  mat::fixed<3, 3> C = lambda*A + B;

  C(find(abs(C) < small)).zeros();

  // Split the degenerate conic into two lines
  mat::fixed<3, 2> lines = split_conic(C);

  // Intersect one of the conics with each line to get 0 to 4 points
  mat::fixed<3, 4> points;
  if (is_finite(lines)) {
    points.cols(0, 1) = intersect_conic_line(A, lines.col(0));
    points.cols(2, 3) = intersect_conic_line(A, lines.col(1));
  } else {
    points.fill(datum::nan);
  }
  return points;
}

#endif
