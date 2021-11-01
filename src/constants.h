#pragma once

#include <RcppArmadillo.h>

const auto SMALL = std::pow(std::numeric_limits<double>::epsilon(), 0.95);
const auto INF   = std::numeric_limits<double>::infinity();
