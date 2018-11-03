#ifndef eulerr_ellipse_h_
#define eulerr_ellipse_h_

#include "helpers.h"
#include "point.h"

class Ellipse {
public:
  Ellipse(double h_, double k_, double a_, double b_, double phi_)
  {
    h = h_;
    k = k_;
    a = std::abs(a_);
    b = std::abs(b_);
    phi = normalize_angle(phi_);
  }

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

  double h, k, a, b, phi;
};

#endif // eulerr_ellipse_h_


