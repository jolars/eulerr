// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

inline double ellipse_area(arma::vec v) {
  return datum::pi * v[2] * v[3];
}

arma::mat translate(arma::vec xy) {
  mat::fixed<3, 3> out;
  out.eye();
  out(span(0, 1), 2) = xy;
  return out;
}

arma::mat rotate(double phi) {
  mat::fixed<3, 3> out;
  out.eye();
  out(0, 0) =  cos(phi);
  out(1, 0) =  sin(phi);
  out(0, 1) = -sin(phi);
  out(1, 1) =  cos(phi);
  return out;
}

// arma::mat standard_to_matrix(arma::vec v) {
//   mat out(3, 3, fill::zeros);
//   mat trans(3, 3, fill::zeros);
//
//   out(0, 0) = 1/pow(v(2), 2);
//   out(1, 1) = 1/pow(v(3), 2);
//   out(2, 2) = -1;
//
//   double theta = v(4);
//   vec xy = -v.subvec(0, 1);
//
//   out = translate(xy).t()*rotate(theta).t()*out*rotate(theta)*translate(xy);
//   return (out + out.t()) / 2;
// }

arma::mat standard_to_matrix(const arma::vec v) {
  double h   = v[0],
         k   = v[1],
         a   = v[2],
         b   = v[3],
         phi = v[4];
  double A, B, C, D, E, F;
  mat::fixed<3, 3> out;
  out.zeros();

  A = pow(a, 2)*pow(sin(phi), 2) + pow(b, 2)*pow(cos(phi), 2);
  B = 2*(pow(b, 2) - pow(a, 2))*sin(phi)*cos(phi);
  C = pow(a, 2)*pow(cos(phi), 2) + pow(b, 2)*pow(sin(phi), 2);
  D = -2*A*h - B*k;
  E = -B*h - 2*C*k;
  F = A*pow(h, 2) + B*h*k + C*pow(k, 2) - pow(a, 2)*pow(b, 2);

  out.at(0, 0) = A;
  out.at(1, 0) = B / 2;
  out.at(1, 1) = C;
  out.at(2, 0) = D / 2;
  out.at(2, 1) = E / 2;
  out.at(2, 2) = F;

  return symmatl(out);
}

arma::vec matrix_to_standard(const arma::mat m) {
  double A = m.at(0, 0),
    B = m.at(1, 0) * 2,
    C = m.at(1, 1),
    D = m.at(2, 0) * 2,
    E = m.at(2, 1) * 2,
    F = m.at(2, 2);
  double h, k, a, b, phi;

  h = (2*C*D - B*E) / (pow(B, 2) - 4*A*C);
  k = (2*A*E - B*D) / (pow(B, 2) - 4*A*C);
  a = (-sqrt(2*(A*pow(E, 2) + C*pow(D, 2) - B*D*E + (pow(B, 2) - 4*A*C)*F)*
    (A + C + sqrt(pow(A - C, 2) + pow(B, 2)))))/(pow(B, 2) - 4*A*C);
  b = (-sqrt(2*(A*pow(E, 2) + C*pow(D, 2) - B*D*E + (pow(B, 2) - 4*A*C)*F)*
    (A + C - sqrt(pow(A - C, 2) + pow(B, 2)))))/(pow(B, 2) - 4*A*C);

  if (B == 0) {
    if (A < C) {
      phi = 0;
    } else {
      phi = datum::pi / 2;
    }
  } else {
    phi = atan2(C - A - sqrt(pow(A - C, 2) + pow(B, 2)), B);
  }

  return {h, k, a, b, phi};
}
