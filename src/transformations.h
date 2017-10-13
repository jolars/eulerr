#ifndef eulerr_transformations_h_
#define eulerr_transformations_h_

#include <RcppArmadillo.h>

// Produce a scaling matrix
inline
arma::mat scale(const arma::vec& xy) {
  arma::mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) = xy(0);
  out(1, 1) = xy(1);
  return out;
}

inline
arma::mat scale(const double x, const double y) {
  arma::mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) = x;
  out(1, 1) = y;
  return out;
}

// Produce a translation matrix
inline
arma::mat translate(const arma::vec& xy) {
  arma::mat::fixed<3, 3> out;
  out.eye();
  out(arma::span(0, 1), 2) = xy;
  return out;
}

inline
arma::mat translate(const double x, const double y) {
  arma::mat::fixed<3, 3> out;
  out.eye();
  out(0, 2) = x;
  out(1, 2) = y;
  return out;
}

// Produce a rotation matrix
inline
arma::mat rotate(const double phi) {
  arma::mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) =  std::cos(phi);
  out(1, 0) = -std::sin(phi);
  out(0, 1) =  std::sin(phi);
  out(1, 1) =  std::cos(phi);
  return out;
}

// Return the adjoint (adjugate) of a 3-by-3 matrix
inline
arma::mat adjoint(const arma::mat& m) {
  arma::mat::fixed<3, 3> out;
  double a = m(0, 0);
  double b = m(1, 0);
  double c = m(1, 1);
  double d = m(2, 0);
  double e = m(2, 1);
  double f = m(2, 2);

  out(0, 0) = c*f - e*e;
  out(1, 0) = d*e - b*f;
  out(1, 1) = a*f - d*d;
  out(2, 0) = b*e - c*d;
  out(2, 1) = b*d - a*e;
  out(2, 2) = a*c - b*b;

  return arma::symmatl(out);
}

// Return the adjoint (adjugate) of a matrix
// inline arma::mat adjoint(const arma::mat& m) {
//   arma::mat::fixed<3, 3> out;
//   arma::mat temp(3, 3);
//   for (arma::uword i = 0; i < 3; i++) {
//     for (arma::uword j = 0; j < 3; j++) {
//       temp = m;
//       temp.shed_col(i);
//       temp.shed_row(j);
//       out(i, j) = std::pow(-1, i + j + 2) * arma::det(temp);
//     }
//   }
//
//   return out;
// }

// Skew-symmetric matrix
inline
arma::cx_mat skewsymmat(const arma::cx_vec& v) {
  arma::cx_mat::fixed<3, 3> out;
  out.diag().zeros();
  out(0, 1) =  v(2);
  out(0, 2) = -v(1);
  out(1, 0) = -v(2);
  out(1, 2) =  v(0);
  out(2, 0) =  v(1);
  out(2, 1) = -v(0);
  return out;
}

// Skew-symmetric matrix for complex matrices
inline
arma::mat skewsymmat(const arma::vec& v) {
  arma::mat::fixed<3, 3> out;
  out.diag().zeros();
  out(0, 1) =  v(2);
  out(0, 2) = -v(1);
  out(1, 0) = -v(2);
  out(1, 2) =  v(0);
  out(2, 0) =  v(1);
  out(2, 1) = -v(0);
  return out;
}

#endif
