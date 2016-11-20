# Compute the overlap of two circles
intersect_two_discs <- function(r1, r2, d) {
  r1 ^ 2L * acos((d ^ 2L + r1 ^ 2L - r2 ^ 2L) / (2L * d * r1)) +
  r2 ^ 2L * acos((d ^ 2L + r2 ^ 2L - r1 ^ 2L) / (2L * d * r2)) -
  sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2)) / 2L
}

# Find intersection points
locate_intersections <- function(r1, r2, x_d, y_d, x_c, y_c, d) {
  l  <- (r2 ^ 2L - r1 ^ 2L + d ^ 2L) / (2L * d)
  h  <- sqrt(r2 ^ 2L - l ^ 2L)
  ld <- l / d
  hd <- h / d
  x1 <- x_d * ld + y_d * hd + x_c
  x2 <- x_d * ld - y_d * hd + x_c
  y1 <- y_d * ld - x_d * hd + y_c
  y2 <- y_d * ld + x_d * hd + y_c
  cbind(c(x1, x2), c(y1, y2))
}

# Find which sets contain some points
find_sets_containing_points <- function(int_points, x, y, r) {
  t(outer(int_points[, 1], x, function(a, x) (a - x) ^ 2) +
      outer(int_points[, 2], y, function(b, y) (b - y) ^ 2)) <= r ^ 2L
}