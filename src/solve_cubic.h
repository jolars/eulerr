#pragma once

#include <RcppArmadillo.h>

// Solve a cubic polynomial
arma::cx_vec
solve_cubic(const double alpha,
            const double beta,
            const double gamma,
            const double delta);
