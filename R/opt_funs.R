initial_layout_optimizer <- function(par, distances, disjoint, contained) {
  x_d <- combn(par[1:(length(par) / 2)], 2, function(x) x[1] - x[2])
  y_d <- combn(par[(1 + length(par) / 2):length(par)], 2, function(x) x[1] - x[2])
  d <- x_d ^ 2 + y_d ^ 2

  loss <- (d - distances ^ 2) ^ 2
  loss[(d >= distances ^ 2) & disjoint] <- 0
  loss[(d <= distances ^ 2) & contained] <- 0

  sum(loss)
}

initial_layout_gradient <- function(par, distances, disjoint, contained) {
  x <- par[1:(length(par) / 2)]
  y <- par[(1 + length(par) / 2):length(par)]

  x_d <- combn(x, 2, function(x) x[1] - x[2])
  y_d <- combn(y, 2, function(x) x[1] - x[2])

  d <- x_d ^ 2 + y_d ^ 2

  grad_x <-
    ifelse((d >= distances ^ 2 & disjoint) | (d <= distances ^ 2 & contained),
           0, 4 * (d - distances ^ 2) * (x_d))
  grad_y <-
    ifelse((d >= distances ^ 2 & disjoint) | (d <= distances ^ 2 & contained),
           0, 4 * (d - distances ^ 2) * (y_d))

  grad_x <- rbind(grad_x, -grad_x)
  grad_y <- rbind(grad_y, -grad_y)

  ind <- combn(length(x), 2)

  c(vapply(seq_along(x),
           function(x) {
             sum(grad_x[ind == x])
           }, FUN.VALUE = double(1)),
    vapply(seq_along(y),
           function(y) {
             sum(grad_y[ind == y])
           }, FUN.VALUE = double(1))
  )
}

# Optimization wrapper for intersect_two_discs when distance is unknown
opt_disc_intersection <- function(x, r1, r2, overlap) {
  (intersect_two_discs(d = x, r1, r2) - overlap) ^ 2
}

# Optimization function for disc_disc intersection
separate_two_discs <- function(r1, r2, overlap) {
  fit <- optimize(
    opt_disc_intersection,
    interval = c(abs(r1 - r2), sum(r1, r2)),
    r1 = r1,
    r2 = r2,
    overlap = overlap
  )
  fit$minimum
}

# Fine tune the initial layout
final_layout_optimizer <- function(par, r, all_areas, oneset_areas,
                                   twoset_areas, twoset_names) {
  x <- par[1:(length(par) / 2)]
  y <- par[(1 + length(par) / 2):length(par)]
  x_c <- combn(x, 2)
  y_c <- combn(y, 2)

  curr_areas <- all_areas
  curr_areas[] <- 0
  curr_oneset_areas <- r ^ 2 * pi

  r_combos <- combn(r, 2)
  x_d      <- combn(x, 2, function(x) (x[1] - x[2]))
  y_d      <- combn(y, 2, function(y) (y[1] - y[2]))
  d        <- sqrt(x_d ^ 2 + y_d ^ 2)

  r1 <- r_combos[1, ]
  r2 <- r_combos[2, ]

  curr_twoset_areas <- twoset_areas

  disjoint     <- d > r1 + r2 | is_equal(d, abs(r1 + r2))
  contained    <- d < abs(r1 - r2)
  intersecting <- !(disjoint | contained)

  curr_twoset_areas[contained]    <- min(r1, r2) ^ 2 * pi
  curr_twoset_areas[disjoint]     <- 0
  curr_twoset_areas[intersecting] <- intersect_two_discs(d = d[intersecting],
                                                         r1 = r1[intersecting],
                                                         r2 = r2[intersecting])

  # Find all points at which circles intersect

  if (any(intersecting) & length(setnames) > 2) {
    d   <- d[intersecting]
    r1  <- r1[intersecting]
    r2  <- r2[intersecting]
    x_d <- x_d[intersecting]
    y_d <- y_d[intersecting]
    x_c <- x_c[2, intersecting]
    y_c <- y_c[2, intersecting]
    on_circles <- twoset_names[intersecting]

    l <- (r2 ^ 2 - r1 ^ 2 + d ^ 2) / (2 * d)
    h <- sqrt(r2 ^ 2 - l ^ 2)

    x_int <- c((l / d) * (x_d) + (h / d) * (y_d) + x_c,
               (l / d) * (x_d) - (h / d) * (y_d) + x_c)
    y_int <- c((l / d) * (y_d) - (h / d) * (x_d) + y_c,
               (l / d) * (y_d) + (h / d) * (x_d) + y_c)
    on_circles <- c(on_circles, on_circles)

    temp_sets <- mapply(find_sets_containing_points,
                        x = x, y = y, r = r,
                        MoreArgs = list(x_int = x_int, y_int = y_int))

    curr_threeplus_areas <- vector("list", length = length(setnames))
    for (i in 3:length(setnames)) {
      ind <- combn(length(setnames), i)
      colnames(ind) <-
        apply(ind, 2, function(x) paste(setnames[x], collapse = "&"))
      curr_threeplus_areas[[i]] <-
        apply(ind, 2,
              function(s) {
                int_sets <-
                  (rowSums(temp_sets[, s]) == i) &
                  (on_circles %in% combn(setnames[s], 2, paste, collapse = "&"))
                if (any(int_sets)) {
                  find_threeplus_areas(x = x_int[int_sets],
                                       y = y_int[int_sets],
                                       these_circles = on_circles[int_sets],
                                       radiuses = r)
                } else {
                  0
                }
              })
    }
    curr_threeplus_areas <- unlist(curr_threeplus_areas)

    curr_areas[names(curr_oneset_areas)]    <- curr_oneset_areas
    curr_areas[names(curr_twoset_areas)]    <- curr_twoset_areas
    curr_areas[names(curr_threeplus_areas)] <- curr_threeplus_areas
  }
  sum((all_areas - curr_areas) ^ 2)
}