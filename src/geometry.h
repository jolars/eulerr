#ifndef eulerr_geometry_
#define eulerr_geometry_

#include <RcppArmadillo.h>

inline
arma::umat
find_surrounding_sets(const arma::rowvec& x,
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

// See which ellipses contain a given set of points
inline
void
adopt(const arma::mat& points,
      const arma::mat& ellipses,
      const arma::uword n,
      const arma::uword i,
      const arma::uword j,
      arma::subview<arma::uword>&& out) {
  for (arma::uword l = 0; l < n; ++l) {
    if ((l == i) | (l == j)) {
      out.row(l).ones();
    } else {
      arma::rowvec x = points.row(0);
      arma::rowvec y = points.row(1);
      double h = ellipses(0, l);
      double k = ellipses(1, l);
      double phi = ellipses(4, l);

      // Check if the points lie inside the ellipse
      out.row(l) = arma::pow((x - h)*std::cos(phi) + (y - k)*std::sin(phi), 2)/
        std::pow(ellipses(2, l), 2) +
          arma::pow((x - h)*std::sin(phi) - (y - k)*std::cos(phi), 2)/
            std::pow(ellipses(3, l), 2) < 1;
    }
  }
}


#endif
