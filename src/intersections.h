// eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
// Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef eulerr_intersections_h_
#define eulerr_intersections_h_

#include <RcppArmadillo.h>
#include "solver.h"
#include "constants.h"
#include "helpers.h"
#include "transformations.h"
#include "point.h"
#include "conic.h"

using namespace arma;

// Split a degenerate conic into two lines
arma::mat
split_conic(const mat& A)
{
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
void
intersect_conic_line(const arma::mat& A,
                     const arma::vec& l,
                     std::vector<Point>& points)
{
  mat::fixed<3, 3> M = skewsymmat(l);
  mat::fixed<3, 3> B = M.t()*A*M;
  vec::fixed<3> l_abs = abs(l);
  mat::fixed<3, 2> out;

  // Pick a non-zero element of l
  if (any(l_abs > SMALL)) {
    uword i = index_max(l_abs);
    uvec li = regspace<uvec>(0, 2);
    li.shed_row(i);

    double alpha = std::sqrt(-det(symmatl(B.submat(li, li))))/l(i);

    mat::fixed<3, 3> C = B + alpha*M;

    vec::fixed<9> C_abs = abs(vectorise(C));

    if (any(C_abs > SMALL)) {
      uvec ind = ind2sub(size(3, 3), index_max(C_abs));
      uword i0 = ind(0);
      uword i1 = ind(1);

      vec::fixed<3> p0 = C.row(i0).t() / C(i0, 2);
      vec::fixed<3> p1 = C.col(i1)     / C(2, i1);

      points.emplace_back(p0[0], p0[1]);
      points.emplace_back(p1[0], p1[1]);
    }
  }
}

std::vector<Point>
intersect(const Conic& conic_A, const Conic& conic_B)
{
  const auto& A = conic_A.M;
  const auto& B = conic_B.M;

  double alpha = det(A);
  double beta = det(join_rows(A.cols(0, 1), B.col(2)))
                + det(join_rows(join_rows(A.col(0), B.col(1)), A.col(2)))
                + det(join_rows(B.col(0), A.cols(1, 2)));
  double gamma = det(join_rows(A.col(0), B.cols(1, 2)))
                 + det(join_rows(join_rows(B.col(0), A.col(1)), B.col(2)))
                 + det(join_rows(B.cols(0, 1), A.col(2)));
  double delta = det(B);

  // Find the cubic roots
  cx_vec::fixed<3> roots = solve_cubic(alpha, beta, gamma, delta);

  // Select the largest real root
  double lambda = 0.0;
  for (auto root : roots) {
    if (nearly_equal(std::imag(root), 0.0)) {
      if (std::abs(std::real(root)) > lambda)
        lambda = std::real(root);
    }
  }

  mat::fixed<3, 3> C = lambda*A + B;

  C(find(abs(C) < SMALL)).zeros();

  // Split the degenerate conic into two lines
  mat::fixed<3, 2> lines = split_conic(C);

  // Intersect one of the conics with each line to get 0 to 4 points
  std::vector<Point> points;
  if (is_finite(lines)) {
    intersect_conic_line(A, lines.col(0), points);
    intersect_conic_line(A, lines.col(1), points);
  }

  return points;
}

#endif // eulerr_intersections_h_
