# Find which sets contain some points
find_sets_containing_points <- function(int_points, x, y, r) {
  t(outer(int_points[, 1], x, function(a, x) (a - x) ^ 2) +
      outer(int_points[, 2], y, function(b, y) (b - y) ^ 2)) <= r ^ 2L
}