#ifndef eulerr_ellipse_h_
#define eulerr_ellipse_h_

#include "helpers.h"

struct Ellipse {
  const double h, k, a, b, phi;

  Ellipse(double h, double k, double a, double b, double phi)
          : h(h),
            k(k),
            a(std::abs(a)),
            b(std::abs(b)),
            phi(normalize_angle(phi))
  {
  }

  double
  area()
  const
  {
    return a*b*PI;
  }

  double
  sector(const double theta)
  const
  {
    return 0.5*a*b*(theta - std::atan2((b - a)*std::sin(2.0*theta),
                                       b + a + (b - a)*std::cos(2.0*theta)));
  }
};

#endif // eulerr_ellipse_h_


