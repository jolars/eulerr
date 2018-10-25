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

template <typename T1, typename T2>
inline
bool
is_subset(const T1& a, const T2& b)
{
  return std::all_of(
    std::begin(a),
    std::end(a),
    [&b](const int k) {
      return std::find(std::begin(b), std::end(b), k) != std::end(b);
    }
  );
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
inline
arma::umat
bit_index(arma::uword n)
{
  using namespace arma;
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

inline
std::vector<std::vector<int>>
set_index(const int n)
{
  std::vector<std::vector<int>> out(std::pow(2, n) - 1);

  for (int i = 1, k = 0; i < n + 1; ++i) {

    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      std::vector<int> ind;
      ind.reserve(i);
      for (int j = 0; j < n; ++j) {
        if (v[j])
          ind.emplace_back(j);
      }
      out[k] = std::move(ind);
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }
  return out;
}

// Signum function
template <typename T>
constexpr int
signum(T x)
{
  return (T(0) < x) - (x < T(0));
}

// Nearly equal
template <typename T>
bool
nearly_equal(T a, T b)
{
  return (std::abs(a - b) <= std::numeric_limits<T>::epsilon() *
          std::max(std::abs(a), std::abs(b)));
}

// Max of minimums colwise
inline
arma::uword
max_colmins(const arma::mat& x)
{
  using namespace arma;
  vec mins(x.n_cols);

  for (uword i = 0; i < x.n_cols; ++i)
    mins(i) = x.col(i).min();

  return mins.index_max();
}

// Convert armadillo vector to rcpp vector
template <typename T>
Rcpp::NumericVector
arma_to_rcpp(const T& x)
{
  using namespace arma;
  return Rcpp::NumericVector(x.begin(), x.end());
}

// Normalize angle
template <typename T>
T
normalize_angle(T& x)
{
  T a = std::fmod(x + PI, 2.0*PI);
  return a >= 0 ? (a - PI) : (a + PI);
}

template <typename T>
constexpr void
clamp(T& x, const double hi, const double lo)
{
  for (auto& x_i : x)
    x_i = x_i < lo ? lo : (x_i > hi ? hi : x_i);
}

template <typename T>
std::vector<T>
seq(const T& n)
{
  std::vector<T> out(n);
  std::iota(std::begin(out), std::end(out), 0);

  return out;
}

template <typename T>
std::vector<T>
seq(const T& from, const T& to)
{
  auto n = static_cast<std::size_t>(to - from);
  std::vector<T> out(n);
  std::iota(std::begin(out), std::end(out), from);

  return out;
}

template <typename T>
constexpr T
pow2(const T& x)
{
  return x*x;
}

#endif
