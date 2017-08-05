#ifndef eulerr_conversions_h_
#define eulerr_conversions_h_

#include "transformations.h"
#include "constants.h"

// arma::mat standard_to_matrix(const arma::vec& v) {
//   arma::mat::fixed<3, 3> out;
//   out.zeros();
//
//   out(0, 0) = std::pow(v(2), -2);
//   out(1, 1) = std::pow(v(3), -2);
//   out(2, 2) = -1;
//
//   double theta = v(4);
//   arma::vec xy = -v.subvec(0, 1);
//
//   out = translate(xy).t()*rotate(theta).t()*out*rotate(theta)*translate(xy);
//   out = (out + out.t())/2;
//   out(arma::find(arma::abs(out) < small)).zeros();
//   return out;
// }

arma::mat standard_to_matrix(const arma::vec& v) {
  double h   = v(0);
  double k   = v(1);
  double a   = v(2);
  double b   = v(3);
  double phi = v(4);
  arma::mat::fixed<3, 3> out;

  double A =
    std::pow(a, 2)*std::pow(sin(phi), 2) + std::pow(b, 2)*std::pow(cos(phi), 2);
  double B = 2*(std::pow(b, 2) - std::pow(a, 2))*std::sin(phi)*std::cos(phi);
  double C =
    std::pow(a, 2)*std::pow(cos(phi), 2) + std::pow(b, 2)*std::pow(sin(phi), 2);
  double D = -2*A*h - B*k;
  double E = -B*h - 2*C*k;
  double F =
    A*std::pow(h, 2) + B*h*k + C*std::pow(k, 2) - std::pow(a, 2)*std::pow(b, 2);

  out(0, 0) = A;
  out(1, 0) = B/2;
  out(1, 1) = C;
  out(2, 0) = D/2;
  out(2, 1) = E/2;
  out(2, 2) = F;

  out(arma::find(arma::abs(out) < small)).zeros();

  return arma::symmatl(out);
}

#endif
