#pragma once

#include <RcppArmadillo.h>
#include "ellipse.h"

namespace eulerr {

struct Conic
{
  arma::mat::fixed<3, 3> M;

  Conic(const eulerr::Ellipse& ellipse);
};
} // namespace eulerr
