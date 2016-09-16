# Compute the overlap of two circles
intersect_two_discs <- function(r1, r2, d) {
  r1 ^ 2 * acos((d ^ 2 + r1 ^ 2 - r2 ^ 2) / (2 * d * r1)) +
    r2 ^ 2 * acos((d ^ 2 + r2 ^ 2 - r1 ^ 2) / (2 * d * r2)) -
    0.5 * sqrt((r1 + r2 - d) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2))
}

# Find intersection points
locate_intersections <- function(r1, r2, x_d, y_d, x_c, y_c, d) {
  l  <- (r2 ^ 2 - r1 ^ 2 + d ^ 2) / (2 * d)
  h  <- sqrt(r2 ^ 2 - l ^ 2)
  ld <- l / d
  hd <- h / d
  x1 <- x_d * ld + y_d * hd + x_c
  x2 <- x_d * ld - y_d * hd + x_c
  y1 <- y_d * ld - x_d * hd + y_c
  y2 <- y_d * ld + x_d * hd + y_c
  cbind(c(x1, x2), c(y1, y2))
}

# Compute the area of a polygon
find_polygon_area <- function(x, y, n) {
  j <- n
  area <- double(n)
  for (i in seq_along(x)) {
    area[i] <- (x[j] + x[i]) * (y[j] - y[i])
    j <- i
  }
  sum(area) / 2
}

# Compute the overlap of three or more circles
find_threeplus_areas <- function(x_int, y_int, radiuses, circles) {
  # Sort points clockwise from center
  j <- n <- length(x_int)
  ind <- order(atan2(x_int - sum(x_int) / n, y_int - sum(y_int) / n))
  x_int <- x_int[ind]
  y_int <- y_int[ind]
  circles <- circles[, ind]

  arc_areas <- double(n)
  for (i in 1:n) {
    circle_now <- circles[, i][circles[, i] %in% circles[, j]]

    d <- sqrt((x_int[j] - x_int[i]) ^ 2 + (y_int[j] - y_int[i]) ^ 2)
    r <- radiuses[circle_now]

    # Find angle from center to segment
    u <- 2 * asin(d / (2 * r))

    # Find area of circle segment, in case we have to competing circles
    A <- ((r ^ 2) / 2) * (u - sin(u))

    # Pick the smallest area in case there are two competing areas
    arc_areas[i] <- A[which.min(A)]

    j <- i
  }
  sum(arc_areas, find_polygon_area(x_int, y_int, n))
}

find_sets_containing_points <- function (points, x, y, r) {
  x_int <- points[1]
  y_int <- points[2]
  (x_int - x) ^ 2 + (y_int - y) ^ 2 < r ^ 2
}
