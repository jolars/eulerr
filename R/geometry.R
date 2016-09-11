# Compute the overlap of two circles
intersect_two_discs <- function(d, r1, r2) {
  r1 ^ 2 * acos((d ^ 2 + r1 ^ 2 - r2 ^ 2) / (2 * d * r1)) +
  r2 ^ 2 * acos((d ^ 2 + r2 ^ 2 - r1 ^ 2) / (2 * d * r2)) -
  .5 * sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2))
}

# Compute the area of a polygon
find_polygon_area <- function(x, y) {
  j <- n <- length(x)
  area <- double(n)
  for (i in seq_along(x)) {
    area[i] <- (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  sum(area) * 0.5
}

# Compute the overlap of three or more circles
find_threeplus_areas <- function(x, y, radiuses, circles, names, pars) {
  # Sort points clockwise from center
  i1 <- order(atan2(x - mean(x), y - mean(y)))
  x <- x[i1]
  y <- y[i1]
  circles <- circles[, i1]

  j <- n <- length(x)
  arc_areas <- double(n)

  for (i in 1:n) {
    c_1 <- circles[, i]
    c_2 <- circles[, j]

    i2 <- c_1[c_1 %in% c_2]

    d <- sqrt((x[j] - x[i]) ^ 2 + (y[j] - y[i]) ^ 2)
    r <- radiuses[i2]

    # Find angle from center to segment
    u <- 2 * asin(d / (2 * r))

    # Find area of circle segment
    arc_areas[i] <- ((r ^ 2) / 2) * (u - sin(u))

    j <- i
  }
  sum(arc_areas, find_polygon_area(x, y))
}

find_sets_containing_points <- function (x_int, y_int, x, y, r) {
  (x_int - x) ^ 2 + (y_int - y) ^ 2 < r ^ 2
}
