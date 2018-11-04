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

#define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include <RcppThread.h>
#include "transformations.h"
#include "intersections.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"

using namespace arma;

struct AreaWorker {
  const arma::mat&  ellipses;
  const arma::umat& id;
  const arma::mat&  points;
  const arma::umat& parents;
  const arma::umat& adopters;
  const bool        approx;

  AreaWorker(const arma::mat&  ellipses,
             const arma::umat& id,
             const arma::mat&  points,
             const arma::umat& parents,
             const arma::umat& adopters,
             const bool        approx)
             : ellipses(ellipses),
               id(id),
               points(points),
               parents(parents),
               adopters(adopters),
               approx(approx) {}

  double operator()(arma::uword i)
  {
    using namespace arma;

    double area = 0.0;

    uvec ids = find(id.row(i));

    if (ids.n_elem == 1) {
      // One set
      area = ellipse_area(ellipses.unsafe_col(ids(0)));
    } else {
      // Two or more sets
      urowvec owners(parents.n_cols);

      for (uword q = 0; q < parents.n_cols; ++q) {
        owners(q) = n_intersections(parents.unsafe_col(q), ids) == 2;
      }

      uvec int_points = find(all(adopters.rows(ids))%owners);

      if (int_points.n_elem == 0) {
        // No intersections: either disjoint or subset
        area = disjoint_or_subset(ellipses.cols(ids));
      } else {
        // Compute the area of the overlap
        bool failure = false;
        area = polysegments(points.cols(int_points),
                            ellipses,
                            parents.cols(int_points),
                            failure);
        if (failure || approx) {
          // Resort to approximation if exact calculation fails
          // TODO: Use a better fallback approximation
          area = montecarlo(ellipses.cols(ids));
        }
      }
    }
    return area;
  }
};

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec intersect_ellipses(const arma::vec& par,
                             const bool       circle,
                             const unsigned   n_threads = 1,
                             const bool       approx = false)
{
  uword n_pars     = circle ? 3 : 5;
  uword n          = par.n_elem/n_pars;
  uword n_int      = 2*n*(n - 1);
  umat  id         = bit_index(n);
  uword n_overlaps = id.n_rows;

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

  for (uword i = 0, k = 0; i < n - 1; ++i) {
    for (uword j = i + 1; j < n; ++j) {
      points.cols(k, k + 3) = intersect_conics(conics.slice(i),
                  conics.slice(j));
      adopters.cols(k, k + 3) = adopt(points.cols(k, k + 3), ellipses, i, j);
      parents(0, span(k, k + 3)).fill(i);
      parents(1, span(k, k + 3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  uvec not_na = find_finite(points.row(0));
  points = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents = parents.cols(not_na);

  // Loop over each set combination
  vec areas(n_overlaps);

  AreaWorker area_worker(ellipses,
                         id,
                         points,
                         parents,
                         adopters,
                         approx);

  RcppThread::ThreadPool pool{n_threads};

  pool.parallelFor(0, n_overlaps, [&area_worker, &areas] (std::size_t i) {
    areas[i] = area_worker(i);
  });

  pool.join();

  vec out(n_overlaps, fill::zeros);

  // hierarchically decompose combination to get disjoint subsets
  for (uword i = id.n_rows; i-- > 0;) {
    out(i) = areas(i) - accu(out(find(all(id.cols(find(id.row(i))), 1))));
  }

  return clamp(out, 0.0, out.max());
}

// stress metric from venneuler (Wilkinson 2012)
// [[Rcpp::export]]
double stress(const arma::vec& orig, const arma::vec& fit)
{
  double sst   = accu(square(fit));
  double slope = accu(orig%fit)/accu(square(orig));
  double sse   = accu(square(fit - orig*slope));
  return sse/sst;
}

// compute loss between the actual and desired areas
// [[Rcpp::export]]
double optim_final_loss(const arma::vec& par,
                        const arma::vec& areas,
                        const bool circle,
                        const int n_threads = 1)
{
  auto fit = intersect_ellipses(par, circle, n_threads, false);

  return accu(square(areas - fit));
}
