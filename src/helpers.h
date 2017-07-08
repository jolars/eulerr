// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Set intersect
inline arma::uvec set_intersect(const arma::urowvec x, const arma::urowvec y) {
  std::vector<int> out;
  std::set_intersection(x.begin(),x.end(), y.begin(), y.end(),
                        std::back_inserter(out));
  return arma::conv_to<arma::uvec>::from(out);
}

template <typename T>
arma::Mat<T> adjoint(const arma::Mat<T> M) {
  arma::Mat<T> out(3, 3);
  T a = M(0, 0),
    b = M(1, 0),
    c = M(1, 1),
    d = M(2, 0),
    e = M(2, 1),
    f = M(2, 2);

  out(0, 0) = c*f - e*e;
  out(1, 0) = d*e - b*f;
  out(1, 1) = a*f - d*d;
  out(2, 0) = b*e - c*d;
  out(2, 1) = b*d - a*e;
  out(2, 2) = a*c - b*b;

  return symmatl(out);
}

template <typename T>
arma::Mat<T> skewsymmat(const arma::Col<T>& v) {
  arma::Mat<T> out(3, 3, fill::zeros);
  out(0, 1) =  v(2);
  out(0, 2) = -v(1);
  out(1, 0) = -v(2);
  out(1, 2) =  v(0);
  out(2, 0) =  v(1);
  out(2, 1) = -v(0);
  return out;
}

// Number of n choose k. (Credited to Ben Voigt.)
inline arma::uword nck(arma::uword n, arma::uword k) {
  if (k > n) return 0;
  if (k * 2 > n) k = n - k;
  if (k == 0) return 1;

  uword out = n;

  for(uword i = 2; i <= k; ++i) {
    out *= (n - i + 1);
    out /= i;
  }
  return out;
}

// Bit indexing
// (http://stackoverflow.com/questions/9430568/generating-combinations-in-c)
template <class C>
arma::umat bit_index(C n) {
  arma::uword n_combos = 0;

  for (arma::uword i = 1; i < n + 1; i++)
    n_combos += nck(n, i);

  arma::umat out(n_combos, n, fill::zeros);

  for (arma::uword i = 1, k = 0; i < n + 1; i++) {
    std::vector<bool> v(n);
    std::fill(v.begin(), v.begin() + i, true);
    do {
      for (uword j = 0; j < n; ++j) {
        if (v[j]) out(k, j) = true;
      }
      k++;
    } while (std::prev_permutation(v.begin(), v.end()));
  }
  return out;
}



