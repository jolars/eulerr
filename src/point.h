#pragma once

namespace eulerr {

struct Point
{
  double h, k;

  Point(const double h, const double k);

  void rotate(const double theta);

  void translate(const double x, const double y);

  void scale(const double x, const double y);
};

}
