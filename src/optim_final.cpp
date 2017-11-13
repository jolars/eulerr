#define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "transformations.h"
#include "intersections.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec intersect_ellipses(const arma::vec& par,
                             const bool circles) {
  arma::uword n_pars   = circles ? 3 : 5;
  arma::uword n        = par.n_elem/n_pars;
  arma::uword n_int    = 2*n*(n - 1);
  arma::umat  id       = bit_index(n);
  arma::uword n_combos = id.n_rows;

  // Set up a matrix of the ellipses in standard form and a cube of conics
  arma::mat ellipses = arma::reshape(par, n_pars, n);

  if (circles) {
    ellipses.insert_rows(3, ellipses.row(2));
    ellipses.insert_rows(4, 1);
  }

  arma::cube conics(3, 3, n);

  for (arma::uword i = 0; i < n; ++i) {
    conics.slice(i) = standard_to_matrix(ellipses.unsafe_col(i));
  }

  // Collect all points of intersection
  arma::mat  points   (3, n_int);
  arma::umat parents  (2, n_int);
  arma::umat adopters (n, n_int);

  for (arma::uword i = 0, k = 0; i < n - 1; ++i) {
    for (arma::uword j = i + 1; j < n; ++j) {
      intersect_conics(conics.slice(i), conics.slice(j), points.cols(k, k + 3));
      adopt(points.cols(k, k + 3), ellipses, i, j, adopters.cols(k, k + 3));
      parents(0, arma::span(k, k + 3)).fill(i);
      parents(1, arma::span(k, k + 3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  arma::uvec not_na = arma::find_finite(points.row(0));
  points   = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents  = parents.cols(not_na);
  arma::uword n_parents = parents.n_cols;
  arma::urowvec owners(n_parents);

  // Loop over each set combination
  arma::vec areas(n_combos);

  for (arma::uword i = 0; i < n_combos; ++i) {
    arma::uvec ids = arma::find(id.row(i));

    if (ids.n_elem == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.unsafe_col(ids(0)));
    } else {
      // Two or more sets
      for (arma::uword q = 0; q < n_parents; ++q) {
        owners(q) = n_intersections(parents.unsafe_col(q), ids) == 2;
      }

      arma::uvec int_points = arma::find(arma::all(adopters.rows(ids))%owners);

      if (int_points.n_elem == 0) {
        // No intersections: either disjoint or subset
        areas(i) = disjoint_or_subset(ellipses.cols(ids));
      } else {
        // Compute the area of the overlap
        bool failure = false;
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points),
                                failure);
        if (failure) {
          // Resort to approximation if exact calculation fails
          // TODO: Use a better fallback approximation
          areas(i) = montecarlo(ellipses.cols(ids));
        }
      }
    }
  }

  arma::vec out(n_combos, arma::fill::zeros);

  for (arma::uword i = n_combos; i-- > 0;) {
    arma::umat subareas = id.cols(arma::find(id.row(i)));
    out(i) = areas(i) - arma::accu(out(arma::find(arma::all(subareas, 1))));
  }

  return arma::clamp(out, 0.0, out.max());
}

// Compute the sums of squares between the actual and desired areas
// [[Rcpp::export]]
double optim_final_loss(const arma::vec& par,
                        const arma::vec& areas,
                        const bool circles) {
  return arma::accu(arma::square(areas - intersect_ellipses(par, circles)));
}
