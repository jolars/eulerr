#pragma once

#include <RcppArmadillo.h>

Rcpp::NumericVector
optim_init(const Rcpp::NumericVector& par,
           const Rcpp::NumericMatrix& d,
           const Rcpp::LogicalMatrix& disjoint,
           const Rcpp::LogicalMatrix& subset);
