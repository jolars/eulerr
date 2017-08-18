#ifndef eulerr_helpers_
#define eulerr_helpers_

#include <RcppArmadillo.h>

// Set intersect
inline arma::uvec set_intersect(const arma::uvec& x, const arma::uvec& y) {
  std::vector<int> out;
  std::set_intersection(x.begin(), x.end(), y.begin(), y.end(),
                        std::back_inserter(out));
  return arma::conv_to<arma::uvec>::from(out);
}

// Number of n choose k. (Credited to Ben Voigt.)
inline arma::uword nck(arma::uword n, arma::uword k) {
  if (k > n)
    return 0;
  if (k*2 > n)
    k = n - k;
  if (k == 0)
    return 1;

  arma::uword out = n;

  for(arma::uword i = 2; i <= k; ++i) {
    out *= (n - i + 1);
    out /= i;
  }
  return out;
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
inline arma::umat bit_index(arma::uword n) {
  arma::uword n_combos = 0;

  for (arma::uword i = 1; i < n + 1; ++i)
    n_combos += nck(n, i);

  arma::umat out(n_combos, n, arma::fill::zeros);

  for (arma::uword i = 1, k = 0; i < n + 1; ++i) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (arma::uword j = 0; j < n; ++j) {
        if (v[j]) out(k, j) = true;
      }
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }
  return out;
}

// Signum function
template <typename T>
inline int sign(T x) {
  return (T(0) < x) - (x < T(0));
}

#endif
