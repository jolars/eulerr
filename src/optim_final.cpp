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
#define ARMA_DONT_USE_OPENMP

#include <RcppArmadillo.h>
#include <RcppParallel.h>
#include "transformations.h"
#include "intersections.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"

using namespace arma;

arma::umat
choose_two(const arma::uword n)
{
  umat m(2, n*(n - 1)/2);
  for (uword i = 0, k = 0; i < n - 1; ++i) {
    for (uword j = i + 1; j < n; ++j, ++k) {
      m(0, k) = i;
      m(1, k) = j;
    }
  }
  return m;
}

struct IntersectionWorker : public RcppParallel::Worker {
  IntersectionWorker(mat& points,
                     umat& parents,
                     umat& adopters,
                     const cube& conics,
                     const mat& ellipses,
                     const umat& two_combos)
                     : points(points),
                       parents(parents),
                       adopters(adopters),
                       conics(conics),
                       ellipses(ellipses),
                       two_combos(two_combos) {}

  void
  operator()(std::size_t begin, std::size_t end)
  {
    for (std::size_t k = begin; k < end; ++k) {
      uword i = two_combos(0, k);
      uword j = two_combos(1, k);
      auto p = intersect_conics(conics.slice(i), conics.slice(j));
      points.cols(k*4, k*4 + 3) = p;
      adopters.cols(k*4, k*4 + 3) = adopt(p, ellipses, i, j);
      parents(0, span(k*4, k*4 + 3)).fill(i);
      parents(1, span(k*4, k*4 + 3)).fill(j);
    }
  }

  mat& points;
  umat& parents;
  umat& adopters;
  const cube& conics;
  const mat& ellipses;
  const umat& two_combos;
};


struct AreaWorker : public RcppParallel::Worker {
  AreaWorker(Rcpp::NumericVector areas,
             const mat& ellipses,
             const umat& id,
             const mat& points,
             const umat& parents,
             const umat& adopters)
             : areas(areas),
               ellipses(ellipses),
               id(id),
               points(points),
               parents(parents),
               adopters(adopters) {}

  void
  operator()(std::size_t begin, std::size_t end)
  {
    std::vector<uword> intersections;
    intersections.reserve(parents.n_rows);

    for (uword i = begin; i < end; ++i) {
      uvec ids = find(id.row(i));

      if (ids.n_elem == 1) {
        // One set
        areas[i] = ellipse_area(ellipses.col(ids(0)));
      } else {
        // Two or more sets
        urowvec owners(parents.n_cols);
        for (uword q = 0; q < parents.n_cols; ++q) {
          std::set_intersection(parents.begin_col(q),
                                parents.end_col(q),
                                ids.begin(),
                                ids.end(),
                                std::back_inserter(intersections));
          owners(q) = intersections.size() == 2;
          intersections.clear();
        }

        uvec int_points = find(all(adopters.rows(ids))%owners);

        if (int_points.n_elem == 0) {
          // No intersections: either disjoint or subset
          areas[i] = disjoint_or_subset(ellipses.cols(ids));
        } else {
          // Compute the area of the overlap
          bool failure = false;
          areas[i] = polysegments(points.cols(int_points),
                                  ellipses,
                                  parents.cols(int_points),
                                  failure);
          if (failure) {
            // Resort to approximation if exact calculation fails
            // TODO: Use a better fallback approximation
            areas[i] = montecarlo(ellipses.cols(ids));
          }
        }
      }
    }
  };

  RcppParallel::RVector<double> areas;
  const mat&  ellipses;
  const umat& id;
  const mat&  points;
  const umat& parents;
  const umat& adopters;
};

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec
intersect_ellipses(const arma::vec& par,
                   const bool circle)
{
  uword n_pars = circle ? 3 : 5;
  uword n      = par.n_elem/n_pars;
  uword n_int  = 2*n*(n - 1);
  umat  id     = bit_index(n);

  // Set up a matrix of the ellipses in standard form and a cube of conics
  mat ellipses = reshape(par, n_pars, n);

  if (circle) {
    ellipses.insert_rows(3, ellipses.row(2));
    ellipses.insert_rows(4, 1);
  }

  // Constrain semiaxes to be positive and angle to [-pi, pi)
  // Necessary if optimizer is unconstrained
  ellipses.rows(2, 3).for_each([](mat::elem_type& x) {x = std::abs(x);});
  ellipses.row(4).for_each([](mat::elem_type& x) {normalize_angle(x);});

  cube conics = standard_to_matrix(ellipses);

  // Collect all points of intersection
  mat  points   (3, n_int);
  umat parents  (2, n_int);
  umat adopters (n, n_int);

  auto two_combos = choose_two(n);

  IntersectionWorker intersection_worker(points,
                                         parents,
                                         adopters,
                                         conics,
                                         ellipses,
                                         two_combos);

  RcppParallel::parallelFor(0, two_combos.n_cols, intersection_worker);

  // Shed points that are NA
  uvec not_na = find_finite(points.row(0));
  points = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents = parents.cols(not_na);

  // Loop over each set combination
  Rcpp::NumericVector areas(id.n_rows);

  AreaWorker area_worker(areas, ellipses, id, points, parents, adopters);

  RcppParallel::parallelFor(0, id.n_rows, area_worker);

  vec out(id.n_rows, fill::zeros);

  // hierarchically decompose combination to get disjoint subsets
  for (uword i = id.n_rows; i-- > 0;)
    out(i) = areas[i] - accu(out(find(all(id.cols(find(id.row(i))), 1))));

  return clamp(out, 0.0, out.max());
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
