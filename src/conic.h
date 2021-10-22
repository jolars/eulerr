#ifndef eulerr_conic_h_
#define eulerr_conic_h_

#include <RcppArmadillo.h>
#include "ellipse.h"

namespace eulerr {

struct Conic
{
  arma::mat::fixed<3, 3> M;

  Conic(const eulerr::Ellipse& ellipse);
};
} // namespace eulerr

#endif // eulerr_conic_h_
