#ifndef eulerr_constants_h
#define eulerr_constants_h

#include <RcppArmadillo.h>

const double small = std::pow(arma::datum::eps, 0.95);
const double two_pi = 2*arma::datum::pi;

#endif
