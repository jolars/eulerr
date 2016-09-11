initial_layout_optimizer <- function(par, distances, disjoint, contained, id) {
  pars <- matrix(par, ncol = 2)
  x    <- pars[, 1]
  y    <- pars[, 2]

  d <- as.vector(dist(cbind(x, y)) ^ 2)

  ind  <- !(((d >= distances ^ 2) & disjoint) | (d <= distances ^ 2 & contained))

  sum((d[ind] - distances[ind] ^ 2) ^ 2)
}

initial_layout_gradient <- function(par, distances, disjoint, contained, id) {
  pars <- matrix(par, ncol = 2)
  x    <- pars[, 1]
  y    <- pars[, 2]
  x_d  <- x[id[[2]][1, ]] - x[id[[2]][2, ]]
  y_d  <- y[id[[2]][1, ]] - y[id[[2]][2, ]]
  d    <- as.vector(dist(cbind(x, y)) ^ 2)
  i1   <- !((d >= distances ^ 2 & disjoint) | (d <= distances ^ 2 & contained))
  grad_x <- grad_y <- double(length(i1))
  grad_x[i1] <- 4 * (d[i1] - distances[i1] ^ 2) * (x_d[i1])
  grad_y[i1] <- 4 * (d[i1] - distances[i1] ^ 2) * (y_d[i1])
  grad_x <- rbind(grad_x, -grad_x)
  grad_y <- rbind(grad_y, -grad_y)
  c(vapply(seq_along(x), function(x) {sum(grad_x[id[[2]] == x])},
           FUN.VALUE = double(1)),
    vapply(seq_along(y), function(y) {sum(grad_y[id[[2]] == y])},
           FUN.VALUE = double(1)))
}

# Optimization wrapper for intersect_two_discs when distance is unknown
opt_disc_intersection <- function(x, r1, r2, overlap) {
  (intersect_two_discs(d = x, r1, r2) - overlap) ^ 2
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
final_layout_optimizer <- function(par, areas, names, id) {
  fit <- return_intersections(par, areas, names, id)
  sum((unlist(fit[c(-1, -2)]) - unlist(areas[c(-1, -2)])) ^ 2)
}

# Return areas from x, y, etc.
return_intersections <- function(par, areas, names, id) {
  pars <- matrix(par, ncol = 3)
  x <- pars[, 1]
  y <- pars[, 2]
  r <- pars[, 3]

  areas[[1]] <- r ^ 2 * pi

  x_c <- matrix(x[id[[2]]], nrow = 2)
  y_c <- matrix(y[id[[2]]], nrow = 2)

  x_d <- x[id[[2]][1, ]] - x[id[[2]][2, ]]
  y_d <- y[id[[2]][1, ]] - y[id[[2]][2, ]]
  d   <- sqrt(x_d ^ 2 + y_d ^ 2)

  r1 <- r[id[[2]][1, ]]
  r2 <- r[id[[2]][2, ]]

  contained    <- d <= abs(r1 - r2)
  disjoint     <- d >= r1 + r2
  intersecting <- !(disjoint | contained)

  areas[[2]][contained]    <- min(r1, r2) ^ 2 * pi
  areas[[2]][disjoint]     <- 0
  areas[[2]][intersecting] <- intersect_two_discs(d  = d[intersecting],
                                                  r1 = r1[intersecting],
                                                  r2 = r2[intersecting])
  # Find all points at which circles intersect
  if (any(intersecting) & length(areas) > 2) {
    d   <- d[intersecting]
    r1  <- r1[intersecting]
    r2  <- r2[intersecting]
    x_d <- x_d[intersecting]
    y_d <- y_d[intersecting]
    x_c <- x_c[2, intersecting]
    y_c <- y_c[2, intersecting]
    circles <- id[[2]][, intersecting]

    l <- (r2 ^ 2 - r1 ^ 2 + d ^ 2) / (2 * d)
    h <- sqrt(r2 ^ 2 - l ^ 2)

    x_int <- c((l / d) * (x_d) + (h / d) * (y_d) + x_c,
               (l / d) * (x_d) - (h / d) * (y_d) + x_c)
    y_int <- c((l / d) * (y_d) - (h / d) * (x_d) + y_c,
               (l / d) * (y_d) + (h / d) * (x_d) + y_c)
    on_circles <- cbind(circles, circles)

    temp_sets <- mapply(find_sets_containing_points, x = x, y = y, r = r,
                        MoreArgs = list(x_int = x_int, y_int = y_int))
    for (i in 1:ncol(on_circles)) temp_sets[i, on_circles[, i]] <- TRUE

    for (i in 3:length(id)) {
      for (j in 1:ncol(id[[i]])) {
        i1 <- id[[i]][, j]
        points <- which(rowSums(temp_sets[, i1]) == i)
        prt <- on_circles[, points]
        find <- apply(prt, 2, function(x) all(x %in% i1))
        ptt <- points[find]
        if (length(ptt) < 2) {
          contA <- double(1)
          for (k in seq_along(i1)) {
            dX <- sqrt((x[i1][k] - x[i1][-k]) ^ 2 + (y[i1][k] - y[i1][-k]) ^ 2)
            it <- dX <= abs(r[i1][k] - r[i1][-k])
            if (all(it)) contA <- areas[[1]][i1][k]
          }
          areas[[i]][j] <- contA
        } else if (length(ptt) == 2) {
          ind <- on_circles[, ptt][, 1]
          areas[[i]][j] <- intersect_two_discs(d = dist(cbind(x[ind], y[ind])),
                                               r1 = r[ind][1],
                                               r2 = r[ind][2])
        } else {
          areas[[i]][j] <-
            find_threeplus_areas(
              x = x_int[ptt],
              y = y_int[ptt],
              r = r,
              pars = pars,
              circles = on_circles[, ptt],
              names = names
            )
        }
      }
    }
  }
  areas
}