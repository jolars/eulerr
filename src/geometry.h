#ifndef eulerr_geometry_
#define eulerr_geometry_

#include <RcppArmadillo.h>

// See if a group of ellipses are completely disjoint or a russian doll
inline
double
disjoint_or_subset(const arma::mat& M) {
  arma::rowvec areas = M.row(2)%M.row(3)*arma::datum::pi;
  arma::uword i = areas.index_min();

  arma::rowvec xmh    = M(0, i) - M.row(0);
  arma::rowvec ymk    = M(1, i) - M.row(1);
  arma::rowvec phi    = M.row(4);
  arma::rowvec cosphi = arma::cos(phi);
  arma::rowvec sinphi = arma::sin(phi);

  arma::urowvec is_subset =
    arma::pow(xmh%cosphi + ymk%sinphi, 2)/arma::pow(M.row(2), 2) +
    arma::pow(xmh%sinphi - ymk%cosphi, 2)/arma::pow(M.row(3), 2) < 1.0;

  is_subset.shed_col(i);

  return arma::all(is_subset) ? areas(i) : 0.0;
}

inline
arma::umat
find_surrounding_sets(const arma::rowvec& x,
                      const arma::rowvec& y,
                      const arma::rowvec& h,
                      const arma::rowvec& k,
                      const arma::rowvec& a,
                      const arma::rowvec& b,
                      const arma::rowvec& phi) {
  arma::umat out(h.n_elem, x.n_elem);

  for (arma::uword i = 0; i < h.n_elem; ++i) {
    double cosphi = std::cos(phi(i));
    double sinphi = std::sin(phi(i));
    double hi = h(i);
    double ki = k(i);
    double ai = a(i);
    double bi = b(i);

    out.row(i) = arma::square((x - hi)*cosphi + (y - ki)*sinphi)/(ai*ai) +
                 arma::square((x - hi)*sinphi - (y - ki)*cosphi)/(bi*bi) < 1.0;
  }
  return out;
}

// See which ellipses contain a given set of points
inline
void
adopt(const arma::mat& points,
      const arma::mat& ellipses,
      const arma::uword i,
      const arma::uword j,
      arma::subview<arma::uword>&& out) {
  for (arma::uword l = 0; l < ellipses.n_cols; ++l) {
    if ((l == i) || (l == j)) {
      out.row(l).ones();
    } else {
      arma::rowvec x = points.row(0);
      arma::rowvec y = points.row(1);
      double h = ellipses(0, l);
      double k = ellipses(1, l);
      double a = ellipses(2, l);
      double b = ellipses(3, l);
      double phi = ellipses(4, l);
      double cosphi = std::cos(phi);
      double sinphi = std::sin(phi);

      // Check if the points lie inside the ellipse
      out.row(l) = arma::pow((x - h)*cosphi + (y - k)*sinphi, 2)/(a*a) +
                   arma::pow((x - h)*sinphi - (y - k)*cosphi, 2)/(b*b) < 1.0;
    }
  }
}

#endif
