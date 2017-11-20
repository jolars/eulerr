//#define ARMA_NO_DEBUG // For the final version

#include <RcppArmadillo.h>
#include "transformations.h"
#include "intersections.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"
#include "areas.h"

using namespace arma;

// Intersect any number of ellipses or circles
// [[Rcpp::export]]
arma::vec
intersect_ellipses(const arma::vec& par,
                   const bool circle) {
  uword n_pars   = circle ? 3 : 5;
  uword n        = par.n_elem/n_pars;
  uword n_int    = 2*n*(n - 1);
  umat  id       = bit_index(n);

  // Set up a matrix of the ellipses in standard form and a cube of conics
  mat ellipses = reshape(par, n_pars, n);

  if (circle) {
    ellipses.insert_rows(3, ellipses.row(2));
    ellipses.insert_rows(4, 1);
  }

  // Constrain semiaxes to be positive and angle to [-pi, pi)
  // Necessary if optimizer is unbounded
  ellipses.rows(2, 3).for_each([](mat::elem_type& x) {x = std::abs(x);});
  ellipses.row(4).for_each([](mat::elem_type& x) {x = normalize_angle(x);});

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
  urowvec owners(parents.n_cols);

  // Loop over each set combination
  vec areas(id.n_rows);

  for (uword i = 0; i < id.n_rows; ++i) {
    uvec ids = find(id.row(i));

    if (ids.n_elem == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.unsafe_col(ids(0)));
    } else {
      // Two or more sets
      for (uword q = 0; q < parents.n_cols; ++q) {
        owners(q) = n_intersections(parents.unsafe_col(q), ids) == 2;
      }

      uvec int_points = find(all(adopters.rows(ids))%owners);

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

  vec out(id.n_rows, fill::zeros);

  for (uword i = id.n_rows; i-- > 0;) {
    out(i) = areas(i) - accu(out(find(all(id.cols(find(id.row(i))), 1))));
  }

  return clamp(out, 0.0, out.max());
}

// Compute the sums of squares between the actual and desired areas
// [[Rcpp::export]]
double
optim_final_loss(const arma::vec& par,
                 const arma::vec& areas,
                 const bool circle) {
  return accu(square(areas - intersect_ellipses(par, circle)));
}

// Set up a Xptr to optim_final_lass for the last-ditch optimizer
// Adapted from http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2013-April/005744.html
class target_function {
private:
  static target_function *target_function_singleton;
  vec areas;
  bool circle;
  double loss;

public:
  void eval(vec& par) {
    this -> loss = optim_final_loss(par, areas, circle);
  };

  void init(vec& areas_in, bool circle) {
    this -> areas = areas_in;
    this -> circle = circle;
  };

  static target_function* get_target_function_singleton() {
    if (target_function_singleton == 0)
      target_function_singleton = new target_function();
    return target_function_singleton;
  };

  static void delete_target_function_singleton(){
    if (target_function_singleton == 0) {
      return;
    } else {
      delete target_function_singleton;
      target_function_singleton = 0;
    }
    return;
  };

  double get_loss() {
    return loss;
  };
};

target_function* target_function::target_function_singleton = 0;

// [[Rcpp::export]]
SEXP
target_function_ptr(SEXP par_in) {
  vec par = Rcpp::as<arma::vec>(par_in);

  target_function* sp = target_function::get_target_function_singleton();

  sp -> eval(par);

  return Rcpp::wrap(sp -> get_loss());
}

// [[Rcpp::export]]
SEXP
init_final_loss_fun(SEXP areas_in, bool circle) {
  target_function::delete_target_function_singleton();
  target_function* sp = target_function::get_target_function_singleton();
  vec areas = Rcpp::as<arma::vec>(areas_in);
  sp -> init(areas, circle);
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP
get_final_loss_ptr() {
  typedef SEXP (*fun_ptr)(SEXP);
  return (Rcpp::XPtr<fun_ptr>(new fun_ptr(&target_function_ptr)));
}
