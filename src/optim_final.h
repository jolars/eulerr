#pragma once

#include <RcppArmadillo.h>

std::vector<double>
intersect_ellipses(const std::vector<double>& par,
                   const bool circle,
                   const bool approx = false);

double
optim_final_loss(const std::vector<double>& par,
                 const std::vector<double>& areas,
                 const bool circle);
