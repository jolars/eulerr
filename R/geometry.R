# Compute the overlap of two circles
intersect_two_discs <- function(d, r1, r2) {
  r1 ^ 2 * acos((d ^ 2 + r1 ^ 2 - r2 ^ 2) / (2 * d * r1)) +
  r2 ^ 2 * acos((d ^ 2 + r2 ^ 2 - r1 ^ 2) / (2 * d * r2)) -
  .5 * sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2))
}

# Compute the area of a polygon
find_polygon_area <- function (x, y) {
  j <- n <- length(x)
  area <- double(n)
  for (i in 1:n) {
    area[i] <- (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  sum(area) * 0.5
}

# Compute the overlap of three or more circles
find_threeplus_areas <- function(x, y, radiuses, circles, oneset_names) {
  # Sort points clockwise from center
  i1 <- atan2(x - mean(x), y - mean(y))
  x <- x[order(i1)]
  y <- y[order(i1)]
  circles <- circles[order(i1)]

  j <- n <- length(x)
  arc_areas <- double(n)

  for (i in 1:n) {
    c_1 <- unlist(strsplit(circles[i], fixed = T, split = "&"))
    c_2 <- unlist(strsplit(circles[j], fixed = T, split = "&"))

    i2 <- ifelse(n > 2, c_1[c_1 %in% c_2], c_1[i])

    d <- sqrt((x[j] - x[i]) ^ 2 + (y[j] - y[i]) ^ 2)
    r <- radiuses[oneset_names == i2]

    # Find angle from center to segment
    u <- 2 * asin(d / (2 * r))

    # Find area of circle segment
    arc_areas[i] <- ((r ^ 2) / 2) * (u - sin(u))

    j <- i
  }
  sum(arc_areas, find_polygon_area(x, y))
}

find_sets_containing_points <- function (x_int, y_int, x, y, r) {
  L <- (x_int - x) ^ 2 + (y_int - y) ^ 2
  R <- r ^ 2
  is_equal(L, R) | L < R
}
