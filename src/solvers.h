// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// Solve a cubic polynomial
arma::cx_vec solve_cubic(arma::vec V) {
  arma::cx_vec::fixed<3> y;
  std::complex<double> i(0.0, 1.0);
  double A, B, theta, Q, R, a, b, c;

  B = 0;
  a = V[1]/V[0];
  b = V[2]/V[0];
  c = V[3]/V[0];
  Q = (pow(a, 2) - 3.0*b)/9.0;
  R = (2.0*pow(a, 3) - 9.0*a*b + 27.0*c)/54.0;

  if (pow(R, 2) < pow(Q, 3)) {
    theta = acos(R/sqrt(pow(Q, 3)));
    y(0) = -2.0*sqrt(Q)*cos(theta/3.0) -  a/3.0;
    y(1) = -2.0*sqrt(Q)*cos((theta + 2.0*datum::pi)/3.0) - a/3.0;
    y(2) = -2.0*sqrt(Q)*cos((theta - 2.0*datum::pi)/3.0) - a/3.0;
  } else {
    A = -copysign(1.0, R)*cbrt(std::abs(R) + sqrt(pow(R, 2) - pow(Q, 3)));
    if (A == 0) {
      B = 0;
    } else {
      B = Q/A;
    }
    y(0) = (A + B) - a/3.0;
    y(1) = -0.5*(A + B) - a/3.0 + sqrt(3.0)*i*(A - B)/2.0;
    y(2) = -0.5*(A + B) - a/3.0 - sqrt(3.0)*i*(A - B)/2.0;
  }
  return y;
}


