#ifndef eulerr_point_h_
#define eulerr_point_h_

namespace eulerr {

struct Point
{
  double h, k;

  Point(const double h, const double k);

  void rotate(const double theta);

  void translate(const double x, const double y);

  void scale(const double x, const double y);
};

} // namespace eulerr

#endif // eulerr_point_h_
