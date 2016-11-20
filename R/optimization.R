# Initial optimizer
initial_layout_optimizer <- function(par, distances, disjoint, contained, two) {
  m <- matrix(par, ncol = 2L)
  x <- m[, 1L]
  y <- m[, 2L]
  d <- (x[two[1L, ]] - x[two[2L, ]]) ^ 2L + (y[two[1L, ]] - y[two[2L, ]]) ^ 2L
  i <- !(((d >= distances ^ 2L) & disjoint) | (d <= distances ^ 2L & contained))
  sum((d[i] - distances[i] ^ 2L) ^ 2L)
}

# Gradient for initial optimizer
initial_layout_gradient <- function(par, distances, disjoint, contained, two) {
  m   <- matrix(par, ncol = 2L)
  x   <- m[, 1L]
  y   <- m[, 2L]
  x_d <- x[two[1L, ]] - x[two[2L, ]]
  y_d <- y[two[1L, ]] - y[two[2L, ]]
  d   <- x_d ^ 2L + y_d ^ 2L
  i   <- !((d >= distances ^ 2L & disjoint) | (d <= distances ^ 2L & contained))

  grad_x    <- grad_y <- double(length(i))
  grad_x[i] <- 4L * (d[i] - distances[i] ^ 2L) * x_d[i]
  grad_y[i] <- 4L * (d[i] - distances[i] ^ 2L) * y_d[i]
  grad_x    <- rbind(grad_x, -grad_x)
  grad_y    <- rbind(grad_y, -grad_y)

  c(vapply(seq_along(x), function(x) sum(grad_x[two == x]), FUN.VALUE = double(1L)),
    vapply(seq_along(y), function(y) sum(grad_y[two == y]), FUN.VALUE = double(1L)))
}

# Optimization function for disc_disc intersection
separate_two_discs <- function(r1, r2, overlap) {
  stats::optimize(
    function(x, r1, r2, overlap) {
      (intersect_two_discs(r1, r2, d = x) - overlap) ^ 2L
    },
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap,
    tol = .Machine$double.eps ^ 0.5
  )$minimum
}

# Optimization wrapper for the final layout
final_layout_optimizer <- function(par, areas, id, two, twos, ones) {
  fit <- return_intersections(par, areas, id, two, twos, ones)
  tss <- sum((areas - areas / length(areas)) ^ 2L)
  sse <- sum((areas - fit) ^ 2L)

  if (tss > 0L) {
    sse / tss
  } else {
    0L #for degenerate cases
  }
}

# Return areas from x, y, etc.
return_intersections <- function(par, areas, id, two, twos, ones) {
  m   <- matrix(par, ncol = 3L)
  x   <- m[, 1L]
  y   <- m[, 2L]
  r   <- m[, 3L]
  two_a <- two[1, ]
  two_b <- two[2, ]

  # Fill in the one set areas
  areas[ones] <- r ^ 2L * pi

  x_c <- matrix(x[two], nrow = 2L)
  y_c <- matrix(y[two], nrow = 2L)
  x_d <- x[two_a] - x[two_b]
  y_d <- y[two_a] - y[two_b]
  d   <- sqrt(x_d ^ 2L + y_d ^ 2L)
  r1  <- r[two_a]
  r2  <- r[two_b]

  # Establish if sets are contained, disjoint, or intersecting
  ct <- d <= abs(r1 - r2)
  dj <- d >= r1 + r2
  is <- !(dj | ct)
  areas[twos][ct] <- pmin.int(r1[ct], r2[ct]) ^ 2L * pi
  areas[twos][dj] <- 0L
  areas[twos][is] <- intersect_two_discs(r1 = r1[is], r2 = r2[is], d = d[is])

  int_points <- matrix(NA, ncol = 2L, nrow = sum(twos) * 2L)
  int_points[is, ] <- locate_intersections(
    r1  = r1[is],
    r2  = r2[is],
    x_d = x_d[is],
    y_d = y_d[is],
    x_c = x_c[2L, is],
    y_c = y_c[2L, is],
    d   = d[is]
  )

  in_circles <-
    t(outer(int_points[, 1L], x, function(a, x) (a - x) ^ 2L) +
      outer(int_points[, 2L], y, function(b, y) (b - y) ^ 2L)) <= r ^ 2L
  in_circles[is.na(in_circles)] <- FALSE

  twoway <- cbind(two[, is], two[, is])
  for (i in seq_along(twoway[1L, ])) {
    in_circles[, is][twoway[, i], i] <- TRUE
  }

  all_circles <- cbind(two, two)

  # Iterate over all 3+ intersections
  for (i in which(rowSums(id) > 2, useNames = FALSE)) {
    a <- which(id[i, ], useNames = FALSE)
    b <- match(two_a, a, FALSE) + match(two_b, a, FALSE) > 0L

    # Which intersection points are within all sets?
    in_all <- colSums(in_circles[a, , drop = FALSE]) == length(a) & b

    circles <- all_circles[, in_all, drop = FALSE]

    if (ncol(circles) < 2L) {
      # No intersections between the sets or one contained
      l <- which.min(r[a])
      dl <- ((x[a][l] - x[a][-l]) ^ 2L +
               (y[a][l] - y[a][-l]) ^ 2L) ^ 2L <= (r[a][l] - r[a][-l]) ^ 2L
      if (all(dl)) {
        areas[i] <- r[a][l] ^ 2L * pi
      } else {
        areas[i] <- 0L
      }
    } else if (ncol(circles) == 2L) {
      # Return 2 circle intersection
      areas[i] <- intersect_two_discs(
        r[circles[1L, 1L]],
        r[circles[2L, 1L]],
        d[in_all][1L]
      )
    } else if (ncol(circles) > 2L) {
      # Return three plus circle intersection
      areas[i] <- more_area(
        x_int = int_points[in_all, 1L],
        y_int = int_points[in_all, 2L],
        radiuses = r,
        circles = circles
      )
    }
  }
  areas
}