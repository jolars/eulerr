# Initial optimizer
initial_layout_optimizer <- function(par, distances, disjoint, contained, two) {
  m <- matrix(par, ncol = 2L)
  x <- m[, 1L]
  y <- m[, 2L]
  d <- (x[two[, 1L]] - x[two[, 2L]]) ^ 2L + (y[two[, 1L]] - y[two[, 2L]]) ^ 2L
  i <- !(((d >= distances ^ 2L) & disjoint) | (d <= distances ^ 2L & contained))
  sum((d[i] - distances[i] ^ 2L) ^ 2L)
}

# Gradient for initial optimizer
initial_layout_gradient <- function(par, distances, disjoint, contained, two) {
  m   <- matrix(par, ncol = 2L)
  x   <- m[, 1L]
  y   <- m[, 2L]
  x_d <- x[two[, 1L]] - x[two[, 2L]]
  y_d <- y[two[, 1L]] - y[two[, 2L]]
  d   <- x_d ^ 2L + y_d ^ 2L
  i   <- !((d >= distances ^ 2L & disjoint) | (d <= distances ^ 2L & contained))

  grad_x    <- grad_y <- double(length(i))
  grad_x[i] <- 4L * (d[i] - distances[i] ^ 2L) * x_d[i]
  grad_y[i] <- 4L * (d[i] - distances[i] ^ 2L) * y_d[i]
  grad_x    <- rbind(grad_x, -grad_x)
  grad_y    <- rbind(grad_y, -grad_y)

  c(vapply(seq_along(x),
           function(x) sum(grad_x[t(two) == x]),
           FUN.VALUE = double(1L)),
    vapply(seq_along(y),
           function(y) sum(grad_y[t(two) == y]),
           FUN.VALUE = double(1L)))
}

# Optimization function for disc_disc intersection
separate_two_discs <- function(r1, r2, overlap) {
  stats::optimize(
    function(x, r1, r2, overlap) {
      (discdisc(r1, r2, d = x) - overlap) ^ 2L
    },
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap,
    tol = sqrt(.Machine$double.eps)
  )$minimum
}

