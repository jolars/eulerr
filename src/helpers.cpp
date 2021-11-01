#include <RcppArmadillo.h>

#include "constants.h"

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

// Max of minimums colwise
arma::uword
max_colmins(const arma::mat& x)
{
  using namespace arma;

  vec mins(x.n_cols);

  for (uword i = 0; i < x.n_cols; ++i)
    mins(i) = x.col(i).min();

  return mins.index_max();
}

arma::umat
choose_two(const arma::uword n)
{
  using namespace arma;

  umat m(2, n * (n - 1) / 2);
  for (uword i = 0, k = 0; i < n - 1; ++i) {
    for (uword j = i + 1; j < n; ++j, ++k) {
      m(0, k) = i;
      m(1, k) = j;
    }
  }
  return m;
}
