#ifndef eulerr_conversions_h_
#define eulerr_conversions_h_

#include "transformations.h"
#include "constants.h"

arma::mat standard_to_matrix(const arma::vec& v) {
  arma::mat::fixed<3, 3> out;
  out.zeros();

  out(0, 0) = std::pow(v(2), -2);
  out(1, 1) = std::pow(v(3), -2);
  out(2, 2) = -1;

  double theta = v(4);
  arma::vec xy = -v.subvec(0, 1);

  out = translate(xy).t()*rotate(theta).t()*out*rotate(theta)*translate(xy);
  out = (out + out.t())/2;
  out(arma::find(arma::abs(out) < small)).zeros();
  return out;
}

// arma::mat standard_to_matrix(const arma::vec& v) {
//   double h   = v(0);
//   double k   = v(1);
//   double a   = v(2);
//   double b   = v(3);
//   double phi = v(4);
//   arma::mat::fixed<3, 3> out;
//
//   double A = pow(a, 2)*pow(sin(phi), 2) + pow(b, 2)*pow(cos(phi), 2);
//   double B = 2*(pow(b, 2) - pow(a, 2))*sin(phi)*cos(phi);
//   double C = pow(a, 2)*pow(cos(phi), 2) + pow(b, 2)*pow(sin(phi), 2);
//   double D = -2*A*h - B*k;
//   double E = -B*h - 2*C*k;
//   double F = A*pow(h, 2) + B*h*k + C*pow(k, 2) - pow(a, 2)*pow(b, 2);
//
//   out(0, 0) = A;
//   out(1, 0) = B / 2;
//   out(1, 1) = C;
//   out(2, 0) = D / 2;
//   out(2, 1) = E / 2;
//   out(2, 2) = F;
//
//   return arma::symmatl(out);
// }

#endif
