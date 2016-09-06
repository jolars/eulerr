# The area of intersectiong between two circles with radiuses r1 and r2
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
  sum(area) / 2
}

# Find the area of intersections with 3 or more circles
find_threeplus_areas <- function(x, y, these_circles, radiuses) {
  x_centroid <- mean(x)
  y_centroid <- mean(y)

  # Sort points clockwise from center

  sort_ind <- atan2(x - x_centroid, y - y_centroid)

  x <- x[order(sort_ind)]
  y <- y[order(sort_ind)]
  these_circles <- these_circles[order(sort_ind)]

  poly_area <- find_polygon_area(x, y)

  j <- n <- length(x)
  arc_areas <- double(n)

  for (i in 1:n) {
    c_1 <- unlist(strsplit(these_circles[i], fixed = T, split = "&"))
    c_2 <- unlist(strsplit(these_circles[j], fixed = T, split = "&"))

    ind <- ifelse(n > 2, c_1[c_1 %in% c_2], c_1[i])

    d <- sqrt((x[j] - x[i]) ^ 2 + (y[j] - y[i]) ^ 2)
    r <- radiuses[names(radiuses) == ind]

    # Find angle from center to segment
    u <- 2 * asin(d / (2 * r))

    # Find area of circle segment
    arc_areas[i] <- ((r ^ 2) / 2) * (u - sin(u))

    j <- i
  }
  sum(arc_areas, poly_area)
}

find_sets_containing_points <- function (x_int, y_int, x, y, r) {
  L <- (x_int - x) ^ 2 + (y_int - y) ^ 2
  R <- r ^ 2
  is_equal(L, R) | L < R
}

is_equal <- function(x, y, tol = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tol
}
