// #define ARMA_NO_DEBUG

#include <RcppArmadillo.h>
#include <RcppThread.h>
#include "constants.h"
#include "point.h"
#include "ellipse.h"
#include "conic.h"
#include "intersections.h"
#include "solver.h"
#include "helpers.h"
#include "areas.h"

struct AreaWorker {
  AreaWorker(const std::vector<eulerr::Ellipse>&    ellipses,
             const std::vector<std::vector<int>>&   id,
             const std::vector<eulerr::Point>&      points,
             const std::vector<std::array<int, 2>>& parents,
             const std::vector<std::vector<int>>&   adopters,
             const bool approx)
             : ellipses(ellipses),
               id(id),
               points(points),
               parents(parents),
               adopters(adopters),
               approx(approx) {}

  double
  operator()(std::size_t i)
  {
    double area = 0.0;

    auto id_i = id[i];

    if (id_i.size() == 1) {
      // One set
      area = ellipses[i].area();
    } else {
      // Two or more sets
      std::vector<int> int_points;

      for (std::size_t j = 0; j < parents.size(); ++j) {
        if (is_subset(parents[j], id_i)) {
          if (is_subset(id_i, adopters[j])) {
            int_points.emplace_back(j);
          }
        }
      }

      if (int_points.empty()) {
        // No intersections: either disjoint or subset
        area = disjoint_or_subset(ellipses, id_i);
      } else {
        // Compute the area of the overlap
        bool failure = false;
        area = polysegments(points, ellipses, parents, int_points, failure);

        if (failure || approx) {
          // Resort to approximation if exact calculation fails
          area = montecarlo(ellipses, id_i);
        }
      }
    }

    return area;
  };

  const std::vector<eulerr::Ellipse>&    ellipses;
  const std::vector<std::vector<int>>&   id;
  const std::vector<eulerr::Point>&      points;
  const std::vector<std::array<int, 2>>& parents;
  const std::vector<std::vector<int>>&   adopters;
  const bool                             approx;
};

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
std::vector<double> intersect_ellipses(const Rcpp::NumericVector par,
                                       const bool                circle,
                                       const unsigned            n_threads = 1,
                                       const bool                approx = false)
{
  int  n_pars     = circle ? 3 : 5;
  int  n          = par.size()/n_pars;
  int  n_overlaps = std::pow(2, n) - 1;
  auto id         = set_index(n);

  std::vector<eulerr::Ellipse> ellipses;

  for (int i = 0; i < n; ++i) {
    if (circle) {
      ellipses.emplace_back(par[i*3],
                            par[i*3 + 1],
                            std::abs(par[i*3 + 2]),
                            std::abs(par[i*3 + 2]),
                            0.0);
    } else {
      ellipses.emplace_back(par[i*5],
                            par[i*5 + 1],
                            std::abs(par[i*5 + 2]),
                            std::abs(par[i*5 + 3]),
                            normalize_angle(par[i*5 + 4]));
    }
  }

  std::vector<eulerr::Conic> conics;
  for (decltype(n) i = 0; i < n; ++i)
    conics.emplace_back(ellipses[i]);

  // Collect all points of intersection
  std::vector<eulerr::Point> points;
  std::vector<std::array<int, 2>> parents;
  std::vector<std::vector<int>> adopters;

  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      auto p = intersect(conics[i], conics[j]);

      for (auto& p_i : p) {
        std::array<int, 2> parent = {i, j};
        parents.push_back(std::move(parent));
        adopters.emplace_back(adopt(p_i, ellipses, i, j));
        points.push_back(std::move(p_i));
      }
    }
  }

  // Loop over each set combination
  AreaWorker area_worker(ellipses,
                         id,
                         points,
                         parents,
                         adopters,
                         approx);

  RcppThread::ThreadPool pool{n_threads};

  std::vector<double> areas(n_overlaps);
  pool.parallelFor(0, n_overlaps, [&area_worker, &areas] (std::size_t i) {
    areas[i] = area_worker(i);
  });

  pool.join();

  std::vector<double> out(areas.begin(), areas.end());

  // hierarchically decompose combination to get disjoint subsets
  for (decltype(n_overlaps) i = n_overlaps; i-- > 0;) {
    for (decltype(n_overlaps) j = i + 1; j < n_overlaps; ++j) {
      if (is_subset(id[i], id[j]))
        out[i] -= out[j];
    }
  }

  // Clamp output to be non-zero
  std::transform(out.begin(), out.end(), out.begin(),
                 [](double& x) { return clamp(x, 0.0, INF); });

  return out;
}

// compute loss between the actual and desired areas
// [[Rcpp::export]]
double optim_final_loss(const Rcpp::NumericVector par,
                        const Rcpp::NumericVector areas,
                        const bool circle,
                        const int n_threads = 1)
{
  auto fit = intersect_ellipses(par, circle, n_threads, false);

  return std::inner_product(fit.begin(),
                            fit.end(),
                            std::begin(areas),
                            0.0,
                            std::plus<double>(),
                            [](double a, double b) { return (a - b)*(a -b); });
}
