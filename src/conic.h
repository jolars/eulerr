#ifndef eulerr_conic_h_
#define eulerr_conic_h_

#include "ellipse.h"
#include "helpers.h"
#include <RcppArmadillo.h>

struct Conic {

  arma::mat::fixed<3, 3> M;

  Conic(const Ellipse& ellipse)
  {
    auto h = ellipse.h;
    auto k = ellipse.k;
    auto a = ellipse.a;
    auto b = ellipse.b;
    auto phi = ellipse.phi;
    auto sin_phi = std::sin(phi);
    auto cos_phi = std::cos(phi);

    auto A = a*a*sin_phi*sin_phi + b*b*cos_phi*cos_phi;
    auto B = 2.0*(b*b - a*a)*sin_phi*cos_phi;
    auto C = a*a*cos_phi*cos_phi + b*b*sin_phi*sin_phi;
    auto D = -2.0*A*h - B*k;
    auto E = -B*h - 2.0*C*k;
    auto F = A*h*h + B*h*k + C*k*k - a*a*b*b;

    B *= 0.5;
    D *= 0.5;
    E *= 0.5;

    M(0, 0) = A;
    M(0, 1) = B;
    M(0, 2) = D;
    M(1, 0) = B;
    M(1, 1) = C;
    M(1, 2) = E;
    M(2, 0) = D;
    M(2, 1) = E;
    M(2, 2) = F;

    M(arma::find(arma::abs(M) < small)).zeros();
  }
};

#endif // eulerr_conic_h_
