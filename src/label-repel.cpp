// This file is a modified copy of
// https://github.com/slowkow/ggrepel/blob/master/src/repel_boxes.cpp
// from the ggrepel R package by Kamil Slowikowski, which is
// licensed under GPL-3. The code in repel_boxes() has been modified
// by commenting-out, deleting, and reorganizing some of the code. Moreover,
// many of the Rcpp export directives have been removed.

#include <Rcpp.h>
using namespace Rcpp;

// Main code for text label placement -----------------------------------------

typedef struct {
  double x, y;
} Point;

Point operator -(const Point& a, const Point& b) {
  Point p = {a.x - b.x, a.y - b.y};
  return p;
}

Point operator +(const Point& a, const Point& b) {
  Point p = {a.x + b.x, a.y + b.y};
  return p;
}

Point operator /(const Point& a, const double& b) {
  Point p = {a.x / b, a.y / b};
  return p;
}

Point operator *(const double& b, const Point& a) {
  Point p = {a.x * b, a.y * b};
  return p;
}

Point operator *(const Point& a, const double& b) {
  Point p = {a.x * b, a.y * b};
  return p;
}

typedef struct {
  double x1, y1, x2, y2;
} Box;

Box operator +(const Box& b, const Point& p) {
  Box c = {b.x1 + p.x, b.y1 + p.y, b.x2 + p.x, b.y2 + p.y};
  return c;
}

//' Move a box into the area specificied by x limits and y limits.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @param xlim A Point with limits on the x axis like \code{c(xmin, xmax)}
//' @param ylim A Point with limits on the y axis like \code{c(xmin, xmax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @noRd
Box put_within_bounds(Box b, Point xlim, Point ylim, double force = 1e-5) {
  //double d;
  //if (b.x1 < xlim.x) {
  //  d = std::max(fabs(b.x1 - xlim.x), 0.02);
  //  b.x1 += force / pow(d, 2);
  //  b.x2 += force / pow(d, 2);
  //} else if (b.x2 > xlim.y) {
  //  d = std::max(fabs(b.x2 - xlim.y), 0.02);
  //  b.x1 -= force / pow(d, 2);
  //  b.x2 -= force / pow(d, 2);
  //}
  //if (b.y1 < ylim.x) {
  //  d = std::max(fabs(b.y1 - ylim.x), 0.02);
  //  b.y1 += force / pow(d, 2);
  //  b.y2 += force / pow(d, 2);
  //} else if (b.y2 > ylim.y) {
  //  d = std::max(fabs(b.y2 - ylim.y), 0.02);
  //  b.y1 -= force / pow(d, 2);
  //  b.y2 -= force / pow(d, 2);
  //}
  double width = std::abs(b.x1 - b.x2);
  double height = std::abs(b.y1 - b.y2);

  if (b.x1 < xlim.x) {
    b.x1 = xlim.x;
    b.x2 = b.x1 + width;
  } else if (b.x2 > xlim.y) {
    b.x2 = xlim.y;
    b.x1 = b.x2 - width;
  }
  if (b.y1 < ylim.x) {
    b.y1 = ylim.x;
    b.y2 = b.y1 + height;
  } else if (b.y2 > ylim.y) {
    b.y2 = ylim.y;
    b.y1 = b.y2 - height;
  }
  return b;
}

//' Get the coordinates of the center of a box.
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
Point centroid(Box b, double hjust, double vjust) {
  Point p = {(b.x1 + (b.x2 - b.x1) * hjust), b.y1 + (b.y2 - b.y1) * vjust};
  return p;
}

//' Test if a box overlaps another box.
//' @param a A box like \code{c(x1, y1, x2, y2)}
//' @param b A box like \code{c(x1, y1, x2, y2)}
//' @noRd
bool overlaps(Box a, Box b) {
  return b.x1 <= a.x2 &&
    b.y1 <= a.y2 &&
    b.x2 >= a.x1 &&
    b.y2 >= a.y1;
}

Point repel_force_both(Point a,
                       Point b,
                       double force = 0.000001)
{
  double dx = std::abs(a.x - b.x);
  double dy = std::abs(a.y - b.y);

  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);

  // Compute a unit vector in the direction of the force.
  Point v = (a - b) / std::sqrt(d2);

  // Divide the force by the squared distance.
  Point f = force * v / d2;

  if (dx > dy) {
    // f.y = f.y * dx / dy;
    f.y = f.y * 2;
  } else {
    // f.x = f.x * dy / dx;
    f.x = f.x * 2;
  }

  return f;
}

Point repel_force_y(Point a,
                    Point b,
                    double force = 0.000001)
{
  double dx = std::abs(a.x - b.x);
  double dy = std::abs(a.y - b.y);

  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);

  // Compute a unit vector in the direction of the force.
  Point v = {0,1};
  if (a.y < b.y) {
    v.y = -1;
  }

  // Divide the force by the distance.
  Point f = force * v / d2 * 2;
  return f;
}

Point repel_force_x(Point a,
                    Point b,
                    double force = 0.000001)
{
  double dx = std::abs(a.x - b.x);
  double dy = std::abs(a.y - b.y);

  // Constrain the minimum distance, so it is never 0.
  double d2 = std::max(dx * dx + dy * dy, 0.0004);

  // Compute a unit vector in the direction of the force.
  Point v = {1,0};
  if (a.x < b.x) {
    v.x = -1;
  }

  // Divide the force by the squared distance.
  Point f = force * v / d2 * 2;

  return f;
}

//' Compute the repulsion force upon point \code{a} from point \code{b}.
//'
//' The force decays with the squared distance between the points, similar
//' to the force of repulsion between magnets.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param direction direction in which to exert force, either "both", "x", or "y"
//' @noRd
Point repel_force(Point a,
                  Point b,
                  double force = 0.000001,
                  std::string direction = "both")
{
  Point out;

  if (direction == "x") {
    out = repel_force_x(a, b, force);
  } else if (direction == "y") {
    out = repel_force_y(a, b, force);
  } else {
    out = repel_force_both(a, b, force);
  }

  return out;
}

Point spring_force_both(Point a,
                        Point b,
                        double force = 0.000001)
{
  Point f = {0, 0};
  Point v = (a - b) ;
  f = force * v;

  return f;
}

Point spring_force_y(Point a,
                     Point b,
                     double force = 0.000001)
{
  Point f = {0, 0};
  Point v = {0, (a.y - b.y)};
  f = force * v;
  return f;
}

Point spring_force_x(
    Point a, Point b, double force = 0.000001
) {
  Point f = {0, 0};
  Point v = {(a.x - b.x), 0};
  f = force * v ;
  return f;
}

//' Compute the spring force upon point \code{a} from point \code{b}.
//'
//' The force increases with the distance between the points, similar
//' to Hooke's law for springs.
//'
//' @param a A point like \code{c(x, y)}
//' @param b A point like \code{c(x, y)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param direction direction in which to exert force, either "both", "x", or "y"
//' @noRd
Point spring_force(Point a,
                   Point b,
                   double force = 0.000001,
                   std::string direction = "both")
{
  Point out;

  if (direction == "x") {
    out = spring_force_x(a, b, force);
  } else if (direction == "y") {
    out = spring_force_y(a, b, force);
  } else {
    out = spring_force_both(a, b, force);
  }

  return out;
}

//' Adjust the layout of a list of potentially overlapping boxes.
//' @param data_points A numeric matrix with rows representing points like
//'   \code{rbind(c(x, y), c(x, y), ...)}
//' @param point_padding_x Padding around each data point on the x axis.
//' @param point_padding_y Padding around each data point on the y axis.
//' @param boxes A numeric matrix with rows representing boxes like
//'   \code{rbind(c(x1, y1, x2, y2), c(x1, y1, x2, y2), ...)}
//' @param xlim A numeric vector representing the limits on the x axis like
//'   \code{c(xmin, xmax)}
//' @param ylim A numeric vector representing the limits on the y axis like
//'   \code{c(ymin, ymax)}
//' @param force Magnitude of the force (defaults to \code{1e-6})
//' @param maxiter Maximum number of iterations to try to resolve overlaps
//'   (defaults to 2000)
//' @noRd
// [[Rcpp::export]]
Rcpp::DataFrame repel_boxes(NumericMatrix data_points,
                            double point_padding_x,
                            double point_padding_y,
                            NumericMatrix boxes,
                            NumericVector xlim,
                            NumericVector ylim,
                            NumericVector hjust,
                            NumericVector vjust,
                            double force_push = 1e-7,
                            double force_pull = 1e-7,
                            int maxiter = 2000,
                            std::string direction = "both")
{
  // n_texts <= n_points
  int n_points = data_points.nrow();
  int n_texts = boxes.nrow();

  int iter = 0;
  bool any_overlaps = true;
  bool i_overlaps = true;

  Point xbounds, ybounds;
  xbounds.x = xlim[0];
  xbounds.y = xlim[1];
  ybounds.x = ylim[0];
  ybounds.y = ylim[1];

  // Each data point gets a bounding box.
  std::vector<Box> DataBoxes(n_points);
  for (int i = 0; i < n_points; i++) {
    DataBoxes[i].x1 = data_points(i, 0) - point_padding_x;
    DataBoxes[i].y1 = data_points(i, 1) - point_padding_y;
    DataBoxes[i].x2 = data_points(i, 0) + point_padding_x;
    DataBoxes[i].y2 = data_points(i, 1) + point_padding_y;
  }

  std::vector<Point> Points(n_points);
  for (int i = 0; i < n_points; i++) {
    Points[i].x = data_points(i, 0);
    Points[i].y = data_points(i, 1);
  }

  // Add a tiny bit of jitter to each text box at the start.
  NumericVector r = rnorm(n_texts, 0, force_push);
  std::vector<Box> TextBoxes(n_texts);
  std::vector<double> ratios(n_texts);
  std::vector<Point> original_centroids(n_texts);

  for (int i = 0; i < n_texts; i++) {
    TextBoxes[i].x1 = boxes(i, 0);
    TextBoxes[i].x2 = boxes(i, 2);
    TextBoxes[i].y1 = boxes(i, 1);
    TextBoxes[i].y2 = boxes(i, 3);
    // Don't add jitter if the user wants to repel in just one direction.
    if (direction != "y") {
      TextBoxes[i].x1 += r[i];
      TextBoxes[i].x2 += r[i];
    }
    if (direction != "x") {
      TextBoxes[i].y1 += r[i];
      TextBoxes[i].y2 += r[i];
    }
    // height over width
    ratios[i] = (TextBoxes[i].y2 - TextBoxes[i].y1)
      / (TextBoxes[i].x2 - TextBoxes[i].x1);
    original_centroids[i] = centroid(TextBoxes[i], hjust[i], vjust[i]);
  }

  std::vector<Point> velocities(n_texts);
  double velocity_decay = 0.7;

  Point f, ci, cj;

  any_overlaps = false;

  // check if there are any overlaps at all
  for (int i = 0; i < n_texts; i++) {

    ci = centroid(TextBoxes[i], hjust[i], vjust[i]);

    for (int j = 0; j < n_points; j++) {

      if (i != j) {
        if (j < n_texts && overlaps(TextBoxes[i], TextBoxes[j]))
          any_overlaps = true;
      }

      if (overlaps(DataBoxes[j], TextBoxes[i]))
        any_overlaps = true;
    }
  }

  while (any_overlaps && iter < maxiter) {
    iter += 1;
    any_overlaps = false;
    // The forces get weaker over time.
    force_push *= 0.99999;
    force_pull *= 0.9999;
    // velocity_decay *= 0.999;

    for (int i = 0; i < n_texts; i++) {
      i_overlaps = false;
      f.x = 0;
      f.y = 0;

      ci = centroid(TextBoxes[i], hjust[i], vjust[i]);

      for (int j = 0; j < n_points; j++) {

        if (i == j) {
          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
          // // Repel the box from its data point.
          // if (overlaps(DataBoxes[i], TextBoxes[i])) {
          //   any_overlaps = true;
          //   i_overlaps = true;
          //   f = f + repel_force(ci, Points[i], force_push, direction);
          // }
        } else {
          // Repel the box from overlapping boxes.
          if (j < n_texts && overlaps(TextBoxes[i], TextBoxes[j])) {
            cj = centroid(TextBoxes[j], hjust[j], vjust[j]);
            any_overlaps = true;
            i_overlaps = true;
            f = f + repel_force(ci, cj, force_push, direction);
          }

          // Skip the data points if the padding is 0.
          if (point_padding_x == 0 && point_padding_y == 0) {
            continue;
          }
        }

        // Repel the box from other data points.
        if (overlaps(DataBoxes[j], TextBoxes[i])) {
          any_overlaps = true;
          i_overlaps = true;
          f = f + repel_force(ci, Points[j], force_push, direction);
        }
      }

      // Pull the box toward its original position.
      if (!i_overlaps) {
        // force_pull *= 0.999;
        f = f + spring_force(
          original_centroids[i], ci, force_pull, direction);
      }

      velocities[i] = velocities[i] * velocity_decay + f;
      TextBoxes[i] = TextBoxes[i] + velocities[i];
      // Put boxes within bounds
      TextBoxes[i] = put_within_bounds(TextBoxes[i], xbounds, ybounds);

    } // loop through all text labels
  } // while any overlaps exist and we haven't reached max iterations

  NumericVector xs(n_texts);
  NumericVector ys(n_texts);

  for (int i = 0; i < n_texts; i++) {
    xs[i] = (TextBoxes[i].x1 + TextBoxes[i].x2) / 2;
    ys[i] = (TextBoxes[i].y1 + TextBoxes[i].y2) / 2;
  }

  return Rcpp::DataFrame::create(Rcpp::Named("x") = xs,
                                 Rcpp::Named("y") = ys);
}

std::vector<double> rescale(std::vector<double> v)
{
  double min_value = *std::min_element(v.begin(), v.end());
  double max_value = *std::max_element(v.begin(), v.end());

  for (unsigned i = 0; i < v.size(); i++) {
    v[i] = (v[i] - min_value) / max_value;
  }

  return v;
}
