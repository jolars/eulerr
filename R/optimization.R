initial_layout_optimizer <- function(par, distances, disjoint, contained, id) {
  pars <- matrix(par, ncol = 2)
  x <- pars[, 1]
  y <- pars[, 2]
  d <- as.vector(dist(cbind(x, y)) ^ 2)
  i <- !(((d >= distances ^ 2) & disjoint) | (d <= distances ^ 2 & contained))
  sum((d[i] - distances[i] ^ 2) ^ 2)
}

initial_layout_gradient <- function(par, distances, disjoint, contained, id) {
  pars <- matrix(par, ncol = 2)
  x    <- pars[, 1]
  y    <- pars[, 2]
  x_d  <- x[id[[2]][1, ]] - x[id[[2]][2, ]]
  y_d  <- y[id[[2]][1, ]] - y[id[[2]][2, ]]
  d    <- x_d ^ 2 + y_d ^ 2
  i    <- !((d >= distances ^ 2 & disjoint) | (d <= distances ^ 2 & contained))

  grad_x    <- grad_y <- double(length(i))
  grad_x[i] <- 4 * (d[i] - distances[i] ^ 2) * (x_d[i])
  grad_y[i] <- 4 * (d[i] - distances[i] ^ 2) * (y_d[i])
  grad_x    <- rbind(grad_x, -grad_x)
  grad_y    <- rbind(grad_y, -grad_y)

  c(vapply(seq_along(x), function(x) {sum(grad_x[id[[2]] == x])},
           FUN.VALUE = double(1)),
    vapply(seq_along(y), function(y) {sum(grad_y[id[[2]] == y])},
           FUN.VALUE = double(1)))
}

# Optimization wrapper for intersect_two_discs when distance is unknown
opt_disc_intersection <- function(x, r1, r2, overlap) {
  (intersect_two_discs(r1, r2, d = x) - overlap) ^ 2
}

# Optimization function for disc_disc intersection
separate_two_discs <- function(r1, r2, overlap) {
  stats::optimize(
    opt_disc_intersection,
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap
  )$minimum
}

# Optimization wrapper for the final layout
final_layout_optimizer <- function(par, areas, id) {
  fit <- return_intersections(par, areas, id)
  sum((unlist(fit) - unlist(areas)) ^ 2)
}

# Return areas from x, y, etc.
return_intersections <- function(par, areas, id) {
  pars <- matrix(par, ncol = 3)
  x <- pars[, 1]
  y <- pars[, 2]
  r <- pars[, 3]

  for (i in seq_along(areas)) areas[[i]] <- 0
  areas[[1]] <- r ^ 2 * pi

  x_c <- matrix(x[id[[2]]], nrow = 2)
  y_c <- matrix(y[id[[2]]], nrow = 2)
  x_d <- x[id[[2]][1, ]] - x[id[[2]][2, ]]
  y_d <- y[id[[2]][1, ]] - y[id[[2]][2, ]]
  d   <- sqrt(x_d ^ 2 + y_d ^ 2)
  r1  <- r[id[[2]][1, ]]
  r2  <- r[id[[2]][2, ]]

  contained    <- is_equal(d, abs(r1 - r2)) | d < abs(r1 - r2)
  disjoint     <- is_equal(d, r1 + r2) | d > r1 + r2
  intersecting <- !(disjoint | contained)

  areas[[2]][contained]    <- pmin(r1[contained], r2[contained]) ^ 2 * pi
  areas[[2]][disjoint]     <- 0
  areas[[2]][intersecting] <- intersect_two_discs(r1 = r1[intersecting],
                                                  r2 = r2[intersecting],
                                                  d  = d[intersecting])

  if (!any(intersecting) | length(areas) < 3) return(areas)

  int_points <- array(NA, dim = c(length(areas[[2]]), 2, 2))
  int_points[intersecting, , ] <- locate_intersections(
    r1  = r1[intersecting],
    r2  = r2[intersecting],
    x_d = x_d[intersecting],
    y_d = y_d[intersecting],
    x_c = x_c[2, intersecting],
    y_c = y_c[2, intersecting],
    d   = d[intersecting]
  )

  for (i in 3:length(id)) {
    for (j in 1:ncol(id[[i]])) {
      a <- id[[i]][, j]
      b <- id[[2]][1, ] %in% a & id[[2]][2, ] %in% a

      # Which of the intersection points are within all sets?
      in_all <- matrix(F, ncol = 2, nrow = length(b))
      in_all[b & intersecting,] <-
        apply(
          int_points[b & intersecting, , , drop = FALSE],
          c(1, 3),
          find_sets_containing_points,
          x = x[a],
          y = y[a],
          r = r[a]
        )

      circles <- cbind(id[[2]][, in_all[, 1] & b, drop = FALSE],
                       id[[2]][, in_all[, 2] & b, drop = FALSE])

      if (ncol(circles) < 2) {
        # Either no interactions between the sets or fully contained
        one_cont <- logical(length(a))
        for (k in seq_along(a)) {
          u <- (id[[2]][1, ] %in% a[k] | id[[2]][2, ] %in% a[k]) & b
          one_cont[k] <- all(contained[u])
        }
        if (any(one_cont)) {
          areas[[i]][j] <- min(r[a]) ^ 2 * pi
        } else {
          areas[[i]][j] <- 0
        }
      } else if (ncol(circles) == 2) {
        # Return 2 circle interaction
        areas[[i]][j] <- intersect_two_discs(
          r[circles[1, 1]],
          r[circles[2, 1]],
          d[b & (in_all[, 1] | in_all[, 2])]
        )
      } else if (ncol(circles) > 2) {
        # Return three plus circle interaction
        areas[[i]][j] <- find_threeplus_areas(
          x_int = c(int_points[in_all[, 1] & b, 1, 1],
                    int_points[in_all[, 2] & b, 1, 2]),
          y_int = c(int_points[in_all[, 1] & b, 2, 1],
                    int_points[in_all[, 2] & b, 2, 2]),
          radiuses = r,
          circles = circles
        )
      }
    }
  }
  areas
}