#pragma once

#include "conic.h"
#include "point.h"
#include <RcppArmadillo.h>

// Split a degenerate conic into two lines
arma::mat
split_conic(const arma::mat& A);

// Intersect a conic with two lines to return 0 to 4 intersection points
void
intersect_conic_line(const arma::mat& A,
                     const arma::vec& l,
                     std::vector<eulerr::Point>& points);

std::vector<eulerr::Point>
intersect(const eulerr::Conic& conic_A, const eulerr::Conic& conic_B);
