initial_layout_optimizer <- function(par, distances, disjoint, contained, two) {
  pars <- matrix(par, ncol = 2)
  x    <- pars[, 1]
  y    <- pars[, 2]
  x_d  <- x[two[1, ]] - x[two[2, ]]
  y_d  <- y[two[1, ]] - y[two[2, ]]
  d    <- x_d ^ 2 + y_d ^ 2
  i <- !(((d >= distances ^ 2) & disjoint) | (d <= distances ^ 2 & contained))
  sum((d[i] - distances[i] ^ 2) ^ 2)
}

initial_layout_gradient <- function(par, distances, disjoint, contained, two) {
  pars <- matrix(par, ncol = 2)
  x    <- pars[, 1]
  y    <- pars[, 2]
  x_d  <- x[two[1, ]] - x[two[2, ]]
  y_d  <- y[two[1, ]] - y[two[2, ]]
  d    <- x_d ^ 2 + y_d ^ 2
  i    <- !((d >= distances ^ 2 & disjoint) | (d <= distances ^ 2 & contained))

  grad_x    <- grad_y <- double(length(i))
  grad_x[i] <- 4 * (d[i] - distances[i] ^ 2) * (x_d[i])
  grad_y[i] <- 4 * (d[i] - distances[i] ^ 2) * (y_d[i])
  grad_x    <- rbind(grad_x, -grad_x)
  grad_y    <- rbind(grad_y, -grad_y)

  c(vapply(seq_along(x), function(x) {sum(grad_x[two == x])},
           FUN.VALUE = double(1)),
    vapply(seq_along(y), function(y) {sum(grad_y[two == y])},
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
  sum((unlist(fit) - unlist(areas)) ^ 2) / sum(unlist(areas) ^ 2)
}

# Return areas from x, y, etc.
return_intersections <- function(par, areas, id) {
  pars <- matrix(par, ncol = 3)
  x <- pars[, 1]
  y <- pars[, 2]
  r <- pars[, 3]

  two <- id[[2]]

  for (i in seq_along(areas)) areas[[i]] <- 0
  areas[[1]] <- r ^ 2 * pi

  x_c <- matrix(x[two], nrow = 2)
  y_c <- matrix(y[two], nrow = 2)
  x_d <- x[two[1, ]] - x[two[2, ]]
  y_d <- y[two[1, ]] - y[two[2, ]]
  d   <- sqrt(x_d ^ 2 + y_d ^ 2)
  r1  <- r[two[1, ]]
  r2  <- r[two[2, ]]

  contained    <- d <= abs(r1 - r2)
  disjoint     <- d >= r1 + r2
  intersecting <- !(disjoint | contained)

  areas[[2]][contained]    <- pmin(r1[contained], r2[contained]) ^ 2 * pi
  areas[[2]][disjoint]     <- 0
  areas[[2]][intersecting] <- intersect_two_discs(r1 = r1[intersecting],
                                                  r2 = r2[intersecting],
                                                  d  = d[intersecting])

  int_points <- matrix(NA, ncol = 2, nrow = length(areas[[2]]) * 2)
  int_points[intersecting, ] <- locate_intersections(
    r1  = r1[intersecting],
    r2  = r2[intersecting],
    x_d = x_d[intersecting],
    y_d = y_d[intersecting],
    x_c = x_c[2, intersecting],
    y_c = y_c[2, intersecting],
    d   = d[intersecting]
  )

  in_circles <- matrix(FALSE, ncol = nrow(int_points), nrow = length(x))
  in_circles[, intersecting] <- apply(int_points[intersecting, ],
                                      1, find_sets_containing_points, x, y, r)

  twoway_int <- cbind(two[, intersecting], two[, intersecting])

  for (i in seq_along(twoway_int[1, ])) {
    in_circles[, intersecting][twoway_int[, i], i] <- TRUE
  }

  all_circles <- cbind(two, two)

  # Iterate over all higher order intersections
  for (i in seq_along(id[-c(1, 2)])) {
    i <- i + 2
    for (j in 1:ncol(id[[i]])) {
      a <- id[[i]][, j]
      b <- two[1, ] %in% a & two[2, ] %in% a

      # Which of the intersection points are within all sets?
      in_all <- .colSums(in_circles[a, , drop = FALSE],
                         m = length(a), n = ncol(in_circles)) == length(a) & b

      circles <- all_circles[, in_all, drop = FALSE]

      if (ncol(circles) < 2) {
        # Either no interactions between the sets or fully contained
        l <- which.min(r[a])

        dl <- (x[a][l] - x[a][-l]) ^ 2 + (y[a][l] - y[a][-l]) ^ 2 <=
              abs(r[a][l] - r[a][-l])

        if (all(dl)) {
          areas[[i]][j] <- r[a][l] ^ 2 * pi
        } else {
          areas[[i]][j] <- 0
        }
      } else if (ncol(circles) == 2) {
        # Return 2 circle interaction
        areas[[i]][j] <- intersect_two_discs(
          r[circles[1, 1]],
          r[circles[2, 1]],
          d[in_all][1]
        )
      } else if (ncol(circles) > 2) {
        # Return three plus circle interaction
        areas[[i]][j] <- find_threeplus_areas(
          x_int = int_points[in_all, 1],
          y_int = int_points[in_all, 2],
          radiuses = r,
          circles = circles
        )
      }
    }
  }
  areas
}