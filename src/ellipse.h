#pragma once

namespace eulerr {

struct Ellipse
{
  double h, k, a, b, phi;

  Ellipse(double h, double k, double a, double b, double phi);

  double area() const;

  double sector(const double theta) const;
};

}
