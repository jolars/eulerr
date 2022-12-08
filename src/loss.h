#pragma once

#include <functional>
#include <string>
#include <vector>

namespace eulerr {

std::function<double(double, double)>
makeLossAggregator(const std::string& type);

std::function<double(double, double)>
makeLoss(const std::string& type,
         const std::vector<double>& data,
         const std::vector<double>& fit);

} // namespace eulerr
