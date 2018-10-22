#ifndef eulerr_point_h_
#define eulerr_point_h_

struct Point {
  double h, k;

  Point(const double h, const double k) : h(h), k(k) {}

  void
  rotate(const double theta)
  {
    auto h0 = h;
    auto k0 = k;

    h = h0*std::cos(theta) - k0*std::sin(theta);
    k = h0*std::sin(theta) + k0*std::cos(theta);
  }

  void
  translate(const double x, const double y)
  {
    h += x;
    k += y;
  }
};

#endif // eulerr_point_h_

