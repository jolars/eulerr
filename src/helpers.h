#pragma once

#include <RcppArmadillo.h>

#include "constants.h"

template<typename T1, typename T2>
inline bool
is_subset(const T1& a, const T2& b)
{
  return std::all_of(a.begin(), a.end(), [&b](const int k) {
    return std::find(b.begin(), b.end(), k) != b.end();
  });
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
arma::umat
bit_index(arma::uword n);

template<typename T>
std::vector<std::vector<T>>
set_index(const T n)
{
  std::vector<std::vector<T>> out(std::pow(2, n) - 1);

  for (T i = 1, k = 0; i < n + 1; ++i) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      std::vector<T> ind;
      ind.reserve(i);
      for (T j = 0; j < n; ++j) {
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
template<typename T>
inline constexpr int
signum(T x)
{
  return (T(0) < x) - (x < T(0));
}

// Nearly equal
template<typename T>
inline bool
nearly_equal(T a, T b)
{
  return (std::abs(a - b) <= std::numeric_limits<T>::epsilon() *
                               std::max(std::abs(a), std::abs(b)));
}

// Max of minimums colwise
arma::uword
max_colmins(const arma::mat& x);

// Convert armadillo vector to rcpp vector
template<typename T>
inline Rcpp::NumericVector
arma_to_rcpp(const T& x)
{
  using namespace arma;
  return Rcpp::NumericVector(x.begin(), x.end());
}

// Normalize angle
template<typename T>
inline T
normalize_angle(T& x)
{
  T a = std::fmod(x + M_PI, 2.0 * M_PI);
  return a >= 0 ? (a - M_PI) : (a + M_PI);
}

template<typename T>
inline constexpr T
clamp(const T& x, const T& lo, const T& hi)
{
  return x < lo ? lo : (x > hi ? hi : x);
}

template<typename T>
inline std::vector<T>
seq(const T& n)
{
  std::vector<T> out(n);
  std::iota(std::begin(out), std::end(out), 0);

  return out;
}

template<typename T>
inline std::vector<T>
seq(const T& from, const T& to)
{
  auto n = static_cast<std::size_t>(to - from);
  std::vector<T> out(n);
  std::iota(std::begin(out), std::end(out), from);

  return out;
}

template<typename T>
inline constexpr T
pow2(const T& x)
{
  return x * x;
}

template<typename T>
inline constexpr std::size_t
min_index(const T& x)
{
  return std::distance(x.begin(), std::min_element(x.begin(), x.end()));
}

arma::umat
choose_two(const arma::uword n);
