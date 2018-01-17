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

#ifndef eulerr_helpers_
#define eulerr_helpers_

#include <RcppArmadillo.h>
#include "constants.h"

using namespace arma;

inline
arma::uvec
set_intersect(const arma::uvec& x, const arma::uvec& y) {
  std::vector<int> out;
  std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
                        std::back_inserter(out));
  return conv_to<uvec>::from(out);
}

// Number of intersections
inline
arma::uword
n_intersections(const arma::uvec& x,
                const arma::uvec& y) {
  std::vector<int> out;
  std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
                        std::back_inserter(out));
  return out.size();
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
inline
arma::umat
bit_index(arma::uword n) {
  umat out(std::pow(2, n) - 1, n);

  for (uword i = 1, k = 0; i < n + 1; ++i) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (uword j = 0; j < n; ++j) {
        out(k, j) = v[j] ? 1 : 0;
      }
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }
  return out;
}

// Signum function
template <typename T>
int
signum(T x) {
  return (T(0) < x) - (x < T(0));
}

// Nearly equal
template <typename T>
bool
nearly_equal(T a, T b) {
  return (std::abs(a - b) <= std::numeric_limits<T>::epsilon() *
          std::max(std::abs(a), std::abs(b)));
}

// Max of minimums colwise
inline
arma::uword
max_colmins(const arma::mat& x) {
  vec mins(x.n_cols);

  for (uword i = 0; i < x.n_cols; ++i)
    mins(i) = x.col(i).min();

  return mins.index_max();
}

// Convert armadillo vector to rcpp vector
template <typename T>
Rcpp::NumericVector
arma_to_rcpp(const T& x) {
  return Rcpp::NumericVector(x.begin(), x.end());
}

// Normalize angle
template <typename T>
T
normalize_angle(T& x) {
  T a = std::fmod(x + datum::pi, two_pi);
  return a >= 0 ? (a - datum::pi) : (a + datum::pi);
}

#endif
