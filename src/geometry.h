#ifndef eulerr_geometry_
#define eulerr_geometry_

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
inline
arma::umat find_surrounding_sets(const arma::rowvec& x,
                                 const arma::rowvec& y,
                                 const arma::vec& h,
                                 const arma::vec& k,
                                 const arma::vec& a,
                                 const arma::vec& b,
                                 const arma::vec& phi) {
  arma::uword n = h.n_elem;
  arma::umat out(n, x.n_elem);

  for (arma::uword i = 0; i < n; ++i) {
    double cosphi = std::cos(phi(i));
    double sinphi = std::sin(phi(i));
    out.row(i) =
      arma::square((x - h(i))*cosphi + (y - k(i))*sinphi)/std::pow(a(i), 2) +
      arma::square((x - h(i))*sinphi - (y - k(i))*cosphi)/std::pow(b(i), 2) < 1;
  }

  return out;
}

#endif
