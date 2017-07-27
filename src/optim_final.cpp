// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

#include <RcppArmadillo.h>
#include "transformations.h"
#include "conversions.h"
#include "solver.h"
#include "helpers.h"
#include "constants.h"

// #define ARMA_NO_DEBUG // For the final version

inline double ellipse_area(const arma::vec& v) {
  return arma::datum::pi * v(2) * v(3);
}


// Split a degenerate conic into two lines.
void split_conic(const arma::mat& A,
                 arma::vec& g,
                 arma::vec& h) {
  arma::mat::fixed<3, 3> B = -adjoint(A);

  // Find non-zero index on the diagonal
  if (arma::any(arma::abs(B.diag()) > small)) {
    arma::uword i = arma::index_max(arma::abs(B.diag()));
    std::complex<double> Bii = std::sqrt(B(i, i));

    if (std::real(Bii) >= 0) {
      arma::cx_mat::fixed<3, 3> C = A + skewsymmat(B.col(i)/Bii);

      if (arma::any(arma::abs(arma::vectorise(C)) > small)) {
        // Extract the lines
        arma::uvec ij =
          arma::ind2sub(arma::size(3, 3),
                        arma::index_max(arma::abs(arma::vectorise(C))));
        g = arma::real(C.row(ij(0)).t());
        h = arma::real(C.col(ij(1)));
      } else {
        g.fill(arma::datum::nan);
        h.fill(arma::datum::nan);
      }
    } else {
      g.fill(arma::datum::nan);
      h.fill(arma::datum::nan);
    }
  } else {
    g.fill(arma::datum::nan);
    h.fill(arma::datum::nan);
  }
}

// Intersect a conic with two lines to return 0 to 4 intersection points.
void intersect_conic_line(const arma::mat& A,
                          arma::vec& l,
                          arma::subview<double>&& points) {
  arma::mat::fixed<3, 3> M = skewsymmat(l);
  arma::mat::fixed<3, 3> B = M.t() * A * M;

  l(arma::find(arma::abs(l)) < small).zeros();
  // Pick a non-zero element of l
  if (arma::any(arma::abs(l) > small)) {
    arma::uword i = arma::index_max(arma::abs(l));
    arma::uvec li = arma::linspace<arma::uvec>(0, 2, 3);
    li.shed_row(i);

    double alpha = std::sqrt(-arma::det(arma::symmatl(B.submat(li, li))))/l(i);

    arma::mat::fixed<3, 3> C = B + alpha*M;

    if (arma::any(arma::abs(arma::vectorise(C)) > small)) {
      arma::uvec ind =
        arma::ind2sub(arma::size(3, 3),
                      arma::index_max(arma::abs(arma::vectorise(C))));
      arma::uword i0 = ind(0);
      arma::uword i1 = ind(1);

      points.col(0) = C.row(i0).t() / C(i0, 2);
      points.col(1) = C.col(i1)     / C(2, i1);
    } else {
      points.fill(arma::datum::nan);
    }
  } else {
    points.fill(arma::datum::nan);
  }
}


// Intersect two conics, returning 0-4 intersection points
void intersect_conics(const arma::mat& A,
                      const arma::mat& B,
                      arma::subview<double>&& points) {
  arma::vec::fixed<4> v;
  v(0) = arma::det(A);
  v(1) = arma::det(arma::join_rows(A.cols(0, 1), B.col(2))) +
    arma::det(arma::join_rows(arma::join_rows(A.col(0), B.col(1)), A.col(2))) +
    arma::det(arma::join_rows(B.col(0), A.cols(1, 2)));
  v(2) = arma::det(arma::join_rows(A.col(0), B.cols(1, 2))) +
    arma::det(arma::join_rows(arma::join_rows(B.col(0), A.col(1)), B.col(2))) +
    arma::det(arma::join_rows(B.cols(0, 1), A.col(2)));
  v(3) = arma::det(B);

  // Find the cubic roots
  arma::cx_vec::fixed<3> roots = solve_cubic(v);

  if (arma::any(arma::imag(roots) == 0)) {
    // Select the largest real root
    double lambda = 0;
    for (auto root : roots) {
      if (std::imag(root) == 0) {
        if (std::abs(std::real(root)) > lambda) {
          lambda = std::real(root);
        }
      }
    }

    arma::mat::fixed<3, 3> C = lambda*A + B;

    C(arma::find(arma::abs(C) < small)).zeros();

    // Split the degenerate conic into lines g and h
    arma::vec::fixed<3> g, h;
    split_conic(C, g, h);

    // Intersect one of the conics with each line to get points p q
    intersect_conic_line(A, g, points.cols(0, 1));
    intersect_conic_line(A, h, points.cols(2, 3));

  } else {
    points.fill(arma::datum::nan);
  }
}


// See which ellipses/circles contain the given points
void adopt(const arma::mat& points,
                 const arma::mat& ellipses,
                 const arma::uword n,
                 const arma::uword i,
                 const arma::uword j,
                 arma::subview<arma::uword>&& out) {
  for (arma::uword l = 0; l < n; l++) {
    if ((l == i) | (l == j)) {
      out.row(l).ones();
    } else {
      arma::rowvec x = points.row(0);
      arma::rowvec y = points.row(1);
      double h = ellipses(0, l);
      double k = ellipses(1, l);
      double phi = ellipses(4, l);

      // Check if the points lie inside the ellipse

      out.row(l) = arma::pow((x - h)*std::cos(phi) + (y - k)*std::sin(phi), 2)/
                     std::pow(ellipses(2, l), 2) +
                   arma::pow((x - h)*std::sin(phi) - (y - k)*std::cos(phi), 2)/
                     std::pow(ellipses(3, l), 2) < 1;
    }
  }
}

// Ellipse sector area
inline arma::vec sector_area(const double a,
                             const double b,
                             const arma::vec& theta) {
  return 0.5*a*b*(theta - arma::atan2((b - a)*arma::sin(2*theta),
                                      b + a + (b - a)*arma::cos(2*theta)));
}

// Compute the area of an ellipse segment.
double ellipse_segment(const arma::vec& ellipse,
                       arma::vec p0,
                       arma::vec p1) {
  arma::vec hk = ellipse.subvec(0, 1);
  double a = ellipse(2);
  double b = ellipse(3);
  double phi = ellipse(4);
  arma::vec::fixed<2> x, y, sector;

  p0 = rotate(phi) * translate(-hk) * p0;
  p1 = rotate(phi) * translate(-hk) * p1;

  x(0) = p0(0);
  x(1) = p1(0);
  y(0) = p0(1);
  y(1) = p1(1);

  // x =  cos(phi)*(x - ellipse(0)) + sin(phi)*(y - ellipse(1));
  // y = -sin(phi)*(x - ellipse(0)) + cos(phi)*(y - ellipse(1));


  // arma::vec::fixed<2> theta;
  //
  // if (y(0) >= 0) {
  //   theta(0) = std::acos(x(0)/a);
  // } else {
  //   theta(0) = 2*arma::datum::pi - std::acos(x(0)/a);
  // }
  //
  // if (y(1) >= 0) {
  //   theta(1) = std::acos(x(1)/a);
  // } else {
  //   theta(1) = 2*arma::datum::pi - std::acos(x(1)/a);
//}



  // if (theta(0) > theta(1)) {
  //   theta(0) -= 2*arma::datum::pi;
  // }
  //
  // double area = 0.5*((theta(1) - theta(0))*a*b) +
  //   0.5*std::copysign(1.0, theta(1) - theta(0) - arma::datum::pi)*
  //   std::abs(x(0)*y(1) - x(1)*y(0));
  //
  // return area;


  // Find the angle to the points from the center of the ellipse.
  // arma::vec theta = arma::atan2(y, x);

  arma::vec theta = arma::atan2(y, x);
  //theta(arma::find(theta < 0)) += 2*arma::datum::pi;

  if (theta(1) < theta(0)) {
    theta(1) += 2*arma::datum::pi;
  }

  // Triangle part of the sector
  double triangle = 0.5*std::abs(x(1)*y(0) - x(0)*y(1));

  double dtheta = theta(1) - theta(0);

  if (dtheta <= arma::datum::pi) {
    sector = sector_area(a, b, theta);
    return sector(1) - sector(0) - triangle;
  } else {
    theta(0) += 2*arma::datum::pi;
    sector = sector_area(a, b, theta);
    return a*b*arma::datum::pi - sector(0) + sector(1) + triangle;
  }
}

double polysegments(arma::mat points,
                    const arma::mat& ellipses,
                    arma::umat parents) {
  arma::vec x_int = points.row(0).t();
  arma::vec y_int = points.row(1).t();
  arma::uword n = points.n_cols;

  // Sort points by their angle to the centroid
  // arma::uvec ind = arma::sort_index(arma::atan2(x_int - arma::accu(x_int)/n,
  //                                               y_int - arma::accu(y_int)/n));
  arma::vec ang = arma::atan2(y_int - arma::accu(y_int)/n,
                              x_int - arma::accu(x_int)/n);
  ang(arma::find(ang < 0)) += 2*arma::datum::pi;
  arma::uvec ind = arma::sort_index(ang);

  // Reorder vectors and matrix based on angles to centroid
  points  = points.cols(ind);
  parents = parents.cols(ind);
  x_int   = x_int(ind);
  y_int   = y_int(ind);
  double area = 0;

  for (arma::uword i = 0, j = n - 1; i < n; i++) {
    // First discover which ellipses the points belong to
    arma::uvec ii = set_intersect(parents.col(j), parents.col(i));
    arma::uword i_n = ii.n_elem;
    arma::vec areas(i_n);

    // Ellipse segment
    for (arma::uword k = 0; k < i_n; k++) {
      areas(k) = ellipse_segment(ellipses.col(ii(k)),
                                 points.col(j),
                                 points.col(i));
    }

    // Triangular segment plus ellipse segment
    area += 0.5*((x_int(j) + x_int(i)) * (y_int(j) - y_int(i))) + areas.min();
    j = i;
  }
  return area;
}

double disjoint_or_subset(arma::mat M) {
  arma::rowvec areas = M.row(2) % M.row(3) * arma::datum::pi;
  arma::uword i = areas.index_min();
  double x = M(0, i);
  double y = M(1, i);
  M.shed_col(i);

  arma::rowvec xmh    = x - M.row(0);
  arma::rowvec ymk    = y - M.row(1);
  arma::rowvec phi    = M.row(4);
  arma::rowvec cosphi = arma::cos(phi);
  arma::rowvec sinphi = arma::sin(phi);

  arma::urowvec is_subset =
    arma::pow(xmh%cosphi + ymk%sinphi, 2)/arma::pow(M.row(2), 2) +
    arma::pow(xmh%sinphi - ymk%cosphi, 2)/arma::pow(M.row(3), 2) < 1;

  return arma::all(is_subset) ? areas(i) : 0;
}

// [[Rcpp::export]]
arma::vec intersect_ellipses(const arma::vec& par,
                             const bool circles) {
  arma::uword n_pars   = circles ? 3 : 5;
  arma::uword n        = par.n_elem / n_pars;
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
  for (arma::uword i = 0; i < n; i++) {
    conics.slice(i) = standard_to_matrix(ellipses.col(i));
  }

  // Collect all points of intersection
  arma::mat  points   (3, n_int);
  arma::umat parents  (2, n_int);
  arma::umat adopters (n, n_int);

  for (arma::uword i = 0, k = 0; i < n - 1; i++) {
    for (arma::uword j = i + 1; j < n; j++) {
      arma::uword kp3 = k + 3;
      intersect_conics(conics.slice(i), conics.slice(j), points.cols(k, kp3));
      adopt(points.cols(k, kp3), ellipses, n, i, j, adopters.cols(k, kp3));
      parents(0, arma::span(k, kp3)).fill(i);
      parents(1, arma::span(k, kp3)).fill(j);
      k += 4;
    }
  }

  // Shed points that are NA
  arma::uvec not_na = arma::find_finite(points.row(0));
  points   = points.cols(not_na);
  adopters = adopters.cols(not_na);
  parents  = parents.cols(not_na);

  // Loop over each set combination
  arma::vec areas(n_combos);
  for (arma::uword i = 0; i < n_combos; i++) {
    arma::uvec ids = arma::find(id.row(i) == 1);

    if (ids.n_elem == 1) {
      // One set
      areas(i) = ellipse_area(ellipses.col(arma::as_scalar(ids)));
    } else {
      // Two or more sets
      arma::uvec owners(parents.n_cols);
      for (arma::uword q = 0; q < parents.n_cols; q++) {
        owners(q) = set_intersect(parents.col(q), ids).n_elem == 2;
      }

      arma::uvec int_points =
        arma::find((arma::sum(adopters.rows(ids)).t() == ids.n_elem)%owners);

      if (int_points.n_elem == 0) {
        // No intersections: either disjoint or subset
        areas(i) = disjoint_or_subset(ellipses.cols(ids));
      } else {
        // Compute the area of the overlap
        areas(i) = polysegments(points.cols(int_points),
                                ellipses,
                                parents.cols(int_points));
      }
    }
  }

  arma::vec out(n_combos, arma::fill::zeros);

  for (arma::uword i = n_combos; i-- > 0;) {
    arma::umat subareas = id.cols(arma::find(id.row(i) == 1));
    out(i) = areas(i) -
      arma::accu(out(arma::find(arma::sum(subareas, 1) == subareas.n_cols)));
  }

  return arma::clamp(out, 0, out.max());
}


// [[Rcpp::export]]
double optim_final_loss(const arma::vec& par,
                        const arma::vec& areas,
                        const bool circles) {
  return arma::accu(arma::square(areas - intersect_ellipses(par, circles)));
}
