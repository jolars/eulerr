#ifndef eulerr_helpers_
#define eulerr_helpers_

#include <RcppArmadillo.h>

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
  arma::umat out(std::pow(2, n) - 1, n);

  for (arma::uword i = 1, k = 0; i < n + 1; ++i) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (arma::uword j = 0; j < n; ++j) {
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
sign(T x) {
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
  arma::uword n = x.n_cols;
  arma::vec mins(n);

  for (arma::uword i = 0; i < n; ++i)
    mins(i) = x.col(i).min();

  return mins.index_max();
}

// Convert armadillo vector to rcpp vector
template <typename T>
Rcpp::NumericVector
arma_to_rcpp(const T& x) {
  return Rcpp::NumericVector(x.begin(), x.end());
}

#endif
