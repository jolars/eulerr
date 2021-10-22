#include <RcppArmadillo.h>
#include "ellipse.h"

namespace eulerr {

Ellipse::Ellipse(double h, double k, double a, double b, double phi)
  : h(h)
  , k(k)
  , a(a)
  , b(b)
  , phi(phi){};

double
Ellipse::area() const
{
  return a * b * M_PI;
}

double
Ellipse::sector(const double theta) const
{
  return 0.5 * a * b *
         (theta - std::atan2((b - a) * std::sin(2.0 * theta),
                             b + a + (b - a) * std::cos(2.0 * theta)));
}

} // namespace eulerr
