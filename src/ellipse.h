#ifndef eulerr_ellipse_h_
#define eulerr_ellipse_h_

#include "helpers.h"
#include "point.h"

struct Ellipse {
  const double h, k, a, b, phi;

  Ellipse(double h, double k, double a, double b, double phi)
          : h(h),
            k(k),
            a(std::abs(a)),
            b(std::abs(b)),
            phi(normalize_angle(phi)) {}

  double area() const
  {
    return a*b*PI;
  }

  // The code below is adapted from "The area of intersecting ellipses" by
  // David Eberly, Geometric Tools, LLC (c) 1998-2016
  double sector(const double theta) const
  {
    using namespace std;

    return 0.5*a*b*(theta - atan2((b - a)*sin(2.0*theta),
                                  b + a + (b - a)*cos(2.0*theta)));
  }
};

#endif // eulerr_ellipse_h_


