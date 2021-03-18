#ifndef eulerr_ellipse_h_
#define eulerr_ellipse_h_

namespace eulerr {

struct Ellipse {
  double h, k, a, b, phi;

  Ellipse(double h, double k, double a, double b, double phi)
          : h(h), k(k), a(a), b(b), phi(phi) {}

  double area() const
  {
    return a*b*PI;
  }

  // The code below is adapted from "The area of intersecting ellipses" by
  // David Eberly, Geometric Tools, LLC (c) 1998-2016
  double sector(const double theta) const
  {
    return 0.5*a*b*(theta - atan2((b - a)*std::sin(2.0*theta),
                                  b + a + (b - a)*std::cos(2.0*theta)));
  }
};

} // namespace eulerr

#endif // eulerr_ellipse_h_


