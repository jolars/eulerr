#ifndef eulerr_conversions_h_
#define eulerr_conversions_h_

#include "transformations.h"
#include "constants.h"

using namespace arma;

inline
arma::cube
standard_to_matrix(const mat& m) {
  rowvec h = m.row(0);
  rowvec k = m.row(1);
  rowvec a = m.row(2);
  rowvec b = m.row(3);
  rowvec phi = m.row(4);

  rowvec A = square(a)%square(sin(phi)) + square(b)%square(cos(phi));
  rowvec B = 2.0*(square(b) - square(a))%sin(phi)%cos(phi);
  rowvec C = square(a)%square(cos(phi)) + square(b)%square(sin(phi));
  rowvec D = -2.0*A%h - B%k;
  rowvec E = -B%h - 2.0*C%k;
  rowvec F = A%square(h) + B%h%k + C%square(k) - square(a)%square(b);

  B *= 0.5;
  D *= 0.5;
  E *= 0.5;

  cube out(3, 3, m.n_cols);

  out.tube(0, 0) = A;
  out.tube(0, 1) = B;
  out.tube(0, 2) = D;
  out.tube(1, 0) = B;
  out.tube(1, 1) = C;
  out.tube(1, 2) = E;
  out.tube(2, 0) = D;
  out.tube(2, 1) = E;
  out.tube(2, 2) = F;

  out(find(abs(out) < small)).zeros();

  return out;
}

#endif
