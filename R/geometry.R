# Calculate the distance from a point to a circle -------------------------

dist_point_circle <- function(x, y, h, k, r) {
  abs(sqrt((h - x) ^ 2L + (k - y) ^ 2L) - r)
}
