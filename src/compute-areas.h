#pragma once

#include <RcppArmadillo.h>

#include "ellipse.h"
#include "point.h"

double
ellipse_segment(const eulerr::Ellipse& e, eulerr::Point p0, eulerr::Point p1);

double
polysegments(const std::vector<eulerr::Point>& points,
             const std::vector<eulerr::Ellipse>& ellipses,
             const std::vector<std::vector<int>>& parents,
             const std::vector<int>& int_points,
             bool& failure);

double
montecarlo(const std::vector<eulerr::Ellipse>& ellipses,
           const std::vector<int>& indices);
