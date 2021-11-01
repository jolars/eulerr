#include "compute-areas.h"
#include "conic.h"
#include "constants.h"
#include "ellipse.h"
#include "geometry.h"
#include "helpers.h"
#include "intersections.h"
#include "point.h"
#include <RcppArmadillo.h>

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
std::vector<double>
intersect_ellipses(const std::vector<double>& par,
                   const bool circle,
                   const bool approx = false)
{
  int n_pars     = circle ? 3 : 5;
  int n          = par.size() / n_pars;
  int n_overlaps = std::pow(2, n) - 1;
  auto id        = set_index(n);

  std::vector<eulerr::Ellipse> ellipses;

  for (decltype(n) i = 0; i < n; ++i) {
    if (circle) {
      ellipses.emplace_back(par[i * 3],
                            par[i * 3 + 1],
                            std::abs(par[i * 3 + 2]),
                            std::abs(par[i * 3 + 2]),
                            0.0);
    } else {
      ellipses.emplace_back(par[i * 5],
                            par[i * 5 + 1],
                            std::abs(par[i * 5 + 2]),
                            std::abs(par[i * 5 + 3]),
                            normalize_angle(par[i * 5 + 4]));
    }
  }

  std::vector<eulerr::Conic> conics;
  for (decltype(n) i = 0; i < n; ++i)
    conics.emplace_back(ellipses[i]);

  // Collect all points of intersection
  std::vector<eulerr::Point> points;
  std::vector<std::vector<int>> parents;
  std::vector<std::vector<int>> adopters;

  for (decltype(n) i = 0; i < n - 1; ++i) {
    for (decltype(n) j = i + 1; j < n; ++j) {
      auto p = intersect(conics[i], conics[j]);

      for (auto& p_i : p) {
        std::vector<int> parent = { i, j };
        parents.emplace_back(std::move(parent));
        adopters.emplace_back(adopt(p_i, ellipses, i, j));
        points.push_back(std::move(p_i));
      }
    }
  }

  std::vector<double> areas;
  areas.reserve(n_overlaps);
  std::vector<int> int_points;

  for (const auto& id_i : id) {
    if (id_i.size() == 1) {
      // One set
      areas.emplace_back(ellipses[id_i[0]].area());
    } else {
      // Two or more sets
      for (std::size_t j = 0; j < parents.size(); ++j) {
        if (is_subset(parents[j], id_i) && is_subset(id_i, adopters[j])) {
          int_points.emplace_back(j);
        }
      }

      if (int_points.empty()) {
        // No intersections: either disjoint or subset
        areas.emplace_back(disjoint_or_subset(ellipses, id_i));
      } else {
        // Compute the area of the overlap
        bool failure = false;
        auto area =
          polysegments(points, ellipses, parents, int_points, failure);

        if (failure || approx) {
          // Resort to approximation if exact calculation fails
          area = montecarlo(ellipses, id_i);
        }

        areas.emplace_back(area);
      }
    }
    int_points.clear();
  }

  std::vector<double> out(areas.begin(), areas.end());

  // hierarchically decompose combination to get disjoint subsets
  for (decltype(n_overlaps) i = n_overlaps; i-- > 0;) {
    for (decltype(n_overlaps) j = i + 1; j < n_overlaps; ++j) {
      if (is_subset(id[i], id[j]))
        out[i] -= out[j];
    }
  }

  // Clamp output to be non-zero
  std::transform(out.begin(), out.end(), out.begin(), [](double& x) {
    return clamp(x, 0.0, INF);
  });

  return out;
}

// compute loss between the actual and desired areas
// [[Rcpp::export]]
double
optim_final_loss(const std::vector<double>& par,
                 const std::vector<double>& areas,
                 const bool circle)
{
  auto fit = intersect_ellipses(par, circle, false);

  // return sums of squared errors
  return std::inner_product(
    fit.begin(),
    fit.end(),
    areas.begin(),
    0.0,
    std::plus<double>(),
    [](double a, double b) { return (a - b) * (a - b); });
}
