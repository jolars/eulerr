// eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
// Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// #define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "transformations.h"
#include "intersections.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"
#include "point.h"

using namespace arma;

struct AreaWorker : public RcppParallel::Worker {
  AreaWorker(std::vector<double>& areas,
             const std::vector<Ellipse>& ellipses,
             const std::vector<std::vector<int>>& id,
             const std::vector<Point>& points,
             const std::vector<std::array<int, 2>>& parents,
             const std::vector<std::vector<int>>& adopters,
             const bool approx)
             : areas(areas),
               ellipses(ellipses),
               id(id),
               points(points),
               parents(parents),
               adopters(adopters),
               approx(approx) {}
  void
  operator()(std::size_t begin, std::size_t end)
  {
    std::vector<uword> intersections;
    intersections.reserve(parents.size());

    for (std::size_t i = begin; i < end; ++i) {
      auto id_i = id[i];
      auto n = id_i.size();

      if (n == 1) {
        // One set
        areas[i] = ellipses[i].area();
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
          areas[i] = disjoint_or_subset(ellipses, id_i);
        } else {
          // Compute the area of the overlap
          bool failure = false;
          areas[i] = polysegments(points,
                                  ellipses,
                                  parents,
                                  int_points,
                                  failure);
          if (failure || approx) {
            // Resort to approximation if exact calculation fails
            areas[i] = montecarlo(ellipses, id_i);
          }
        }
      }
    }
  };

  std::vector<double>& areas;
  const std::vector<Ellipse>& ellipses;
  const std::vector<std::vector<int>>& id;
  const std::vector<Point>& points;
  const std::vector<std::array<int, 2>>& parents;
  const std::vector<std::vector<int>>& adopters;
  const bool approx;
};

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec
intersect_ellipses(const arma::vec& par,
                   const bool circle,
                   const bool approx = false)
{
  int  n_pars     = circle ? 3 : 5;
  int  n          = par.n_elem/n_pars;
  int  n_overlaps = std::pow(2, n) - 1;
  auto id         = set_index(n);

  std::vector<Ellipse> ellipses;

  for (int i = 0; i < n; ++i) {
    if (circle) {
      ellipses.emplace_back(par[i*3],
                            par[i*3 + 1],
                            par[i*3 + 2],
                            par[i*3 + 2],
                            0.0);
    } else {
      ellipses.emplace_back(par[i*5],
                            par[i*5 + 1],
                            par[i*5 + 2],
                            par[i*5 + 3],
                            par[i*5 + 4]);
    }
  }

  std::vector<Conic> conics;
  for (int i = 0; i < n; ++i)
    conics.emplace_back(ellipses[i]);

  // Collect all points of intersection
  std::vector<Point> points;
  std::vector<std::array<int, 2>> parents;
  std::vector<std::vector<int>> adopters;

  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      auto p = intersect(conics[i], conics[j]);

      for (auto& p_i : p) {
        std::array<int, 2> parent = {i, j};
        parents.push_back(std::move(parent));
        adopters.emplace_back(adopt(p_i.h, p_i.k, ellipses, i, j));
        points.push_back(std::move(p_i));
      }
    }
  }

  // Loop over each set combination
  std::vector<double> areas(n_overlaps);

  AreaWorker area_worker(areas,
                         ellipses,
                         id,
                         points,
                         parents,
                         adopters,
                         approx);

  RcppParallel::parallelFor(0, n_overlaps, area_worker);

  std::vector<double> out(areas.begin(), areas.end());

  // hierarchically decompose combination to get disjoint subsets
  for (int i = n_overlaps; i-- > 0;) {
    for (int j = i + 1; j < n_overlaps; ++j) {
      if (is_subset(id[i], id[j]))
        out[i] -= out[j];
    }
  }

  // Clamp output to be non-zero
  std::transform(out.begin(), out.end(), out.begin(),
                 [](double& x) { return clamp(x, 0.0, INF); });

  return out;
}

// stress metric from venneuler (Wilkinson 2012)
// [[Rcpp::export]]
double
stress(const arma::vec& orig,
       const arma::vec& fit)
{
  double sst   = accu(square(fit));
  double slope = accu(orig%fit)/accu(square(orig));
  double sse   = accu(square(fit - orig*slope));
  return sse/sst;
}

// compute loss between the actual and desired areas
// [[Rcpp::export]]
double
optim_final_loss(const arma::vec& par,
                 const arma::vec& areas,
                 const bool circle)
{
  return accu(square(areas - intersect_ellipses(par, circle)));
}
