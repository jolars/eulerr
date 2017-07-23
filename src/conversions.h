#ifndef CONVERSIONS_H_
#define CONVERSIONS_H_

#include "transformations.h"

arma::mat::fixed<3, 3> standard_to_matrix(const arma::vec& v) {
  arma::mat::fixed<3, 3> out;
  out.zeros();

  out(0, 0) = pow(v(2), -2);
  out(1, 1) = pow(v(3), -2);
  out(2, 2) = -1;

  double theta = v(4);
  arma::vec xy = -v.subvec(0, 1);

  out = translate(xy).t()*rotate(theta).t()*out*rotate(theta)*translate(xy);
  out = (out + out.t())/2;
  out(arma::find(arma::abs(out) < sqrt(arma::datum::eps))).zeros();
  return out;
}

#endif // CONVERSIONS_H_
