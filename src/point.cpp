#include <RcppArmadillo.h>
#include "point.h"

namespace eulerr {

Point::Point(const double h, const double k)
  : h(h)
  , k(k)
{}

void
Point::rotate(const double theta)
{
  auto h0 = h;
  auto k0 = k;

  h = h0 * std::cos(theta) - k0 * std::sin(theta);
  k = h0 * std::sin(theta) + k0 * std::cos(theta);
}

void
Point::translate(const double x, const double y)
{
  h += x;
  k += y;
}

void
Point::scale(const double x, const double y)
{
  h *= x;
  k *= y;
}

} // namespace eulerr
