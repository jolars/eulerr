#include <cmath>
#include <functional>
#include <numeric>
#include <string>
#include <vector>

namespace eulerr {

std::function<double(double, double)>
makeLossAggregator(const std::string& type)
{
  if (type == "sum")
    return std::plus<double>();
  else if (type == "max")
    return
      [](const double& data, const double& fit) { return std::max(data, fit); };

  // TODO(jolars): return an error code here

  return std::plus<double>();
}

std::function<double(double, double)>
makeLoss(const std::string& type,
         const std::vector<double>& data,
         const std::vector<double>& fit)
{
  if (type == "square") {
    return [](const double& data, const double& fit) {
      return std::pow(data - fit, 2);
    };
  } else if (type == "abs") {
    return [](const double& data, const double& fit) {
      return std::abs(data - fit);
    };
  } else if (type == "region") {
    double sum_data = std::accumulate(data.begin(), data.end(), 0.0);
    double sum_fit  = std::accumulate(fit.begin(), fit.end(), 0.0);

    return [sum_data, sum_fit](const double& data, const double& fit) {
      return std::abs(data / sum_data - fit / sum_fit);
    };
  }

  // TODO(jolars): return an error code here

  return [](const double& data, const double& fit) {
    return std::pow(data - fit, 2);
  };
}

} // namespace eulerr
