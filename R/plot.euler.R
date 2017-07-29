#' Plot Area-Proportional Euler Diagrams
#'
#' Plot Euler diagrams with trellis graphics from \pkg{lattice}. This function
#' calls [lattice::xyplot()] under the hood, allowing plots of
#' both individual euler diagrams as well as grids of diagrams
#' in case the `by` argument was used in the call to [euler()].
#'
#' Almost all of the relevant functionality for [lattice::xyplot()] is
#' available here. For instance, providing specifications to
#' `par.settings` will have an effect on many aspects of the plot. Moreover,
#' arguments that are given here will trickle down to the panel function
#' [panel.euler()] and subsequently to [panel.euler.circles()] and
#' [panel.euler.labels()], which do the actual plotting.
#'
#' The default value for `fill` causes, \pkg{eulerr} to choose color palettes
#' based on the number of sets, trying to provide palettes adapted to color
#' vision deficiencies based on [qualpalr::qualpal()].
#'
#' @param x An object of class `euler`.
#' @param fill Fill color. Either a function that takes as its first argument
#'   the number of colors to generate, or a sequence of colors.
#' @param fill_alpha Alpha for the fill.
#' @param auto.key Plot a legend for the sets.
#' @param counts Plot counts.
#' @param labels A list or character vector of labels.
#' @param fontface Fontface for the labels. (See [grid::gpar()]).
#' @param default.scales Default scales. Turns off
#' @param panel The panel function. Should usually be left untouched.
#' @param par.settings Graphical parameters for trellis displays. See
#'   [lattice::trellis.par.get()].
#' @param default.prepanel Default prepanel function. Should usually be left
#'   untouched.
#' @param ... Arguments to pass down to [panel.euler()], which in turn passes
#'   them down to [panel.euler.circles()] and [panel.euler.labels()].
#' @param outer_strips Deprecated
#' @param fill_opacity Deprecated
#'
#' @inherit lattice::levelplot return
#'
#' @export
#'
#' @seealso [panel.euler.circles()], [panel.euler.labels()],
#'   [lattice::xyplot()], [grid::gpar()], [grid::grid.circle()],
#'   [lattice::panel.xyplot()], [euler()], [qualpalr::qualpal()]
#'
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
#' plot(fit, labels = c("foo", "bar"), fill_alpha = 0.7)
#'
#' # Customize colors, remove borders, bump alpha, color labels white
#' plot(fit,
#'      fill_alpha = 0.5,
#'      fill = c("red", "steelblue4"),
#'      col = "white",
#'      border = "transparent",
#'      fontface = "bold.italic")
#'
#' # Add counts to the plot
#' plot(fit, counts = TRUE)
#'
#' # Add a custom legend and retain counts
#' plot(fit, counts = TRUE, auto.key = list(space = "bottom", columns = 2))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit, lty = c("solid", "dotted"), fill = "transparent", cex = 2,
#'      fontface = 2, labels = c("foo", "bar"))
#'
#' # Plot a grid of euler plots
#' dat <- data.frame(
#'   A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' gridfit <- euler(dat[, 1:2], by = dat[, 3:4])
#' plot(gridfit, auto.key = TRUE)
#'
#' # We can modify the grid layout as well
#' plot(gridfit, layout = c(1, 4))
plot.euler <- function(
    x,
    fill = qualpalr_pal,
    fill_alpha = 0.4,
    auto.key = FALSE,
    counts = FALSE,
    labels = is.logical(auto.key) && !isTRUE(auto.key),
    fontface = "bold",
    par.settings = list(),
    ...,
    default.prepanel = prepanel.euler,
    default.scales = list(draw = FALSE),
    panel = panel.euler,
    outer_strips,
    fill_opacity
  ) {
  assertthat::assert_that(assertthat::is.number(fill_alpha),
                          assertthat::is.flag(auto.key) || is.list(auto.key),
                          assertthat::is.flag(counts) || is.list(counts),
                          is.list(par.settings))

  if (!missing(fill_opacity))
    fill_alpha <- fill_opacity

  if (!missing(outer_strips)) {
    warning("'outer_strips' is deprecated; try latticeExtra::useOuterStrips() for the same functionality.")
  }

  is_by <- inherits(x, "by")

  if (is.function(fill))
    fill <- fill(if (is_by) nrow(x[[1]]$coefficients) else nrow(x$coefficients))

  fill <- grDevices::adjustcolor(fill, fill_alpha)

  if (is_by) {
    dd <- do.call(rbind, lapply(x, "[[", "coefficients"))
    orig <- do.call(cbind, lapply(x, "[[", "original.values"))
    fitted <- do.call(cbind, lapply(x, "[[", "fitted.values"))
    if (isTRUE(labels))
      labels <- rownames(x[[1]]$coefficients)
    else if (is.logical(labels))
      labels <- NULL
  } else {
    dd <- x$coefficients
    orig <- x$original.values
    fitted <- x$fitted.values
    if (isTRUE(labels))
      labels <- rownames(x$coefficients)
    else if (is.logical(labels))
      labels <- NULL
    par.settings <- update_list(par.settings, list(
      axis.line = list(col = "transparent")
    ))
  }

  par.settings <- update_list(par.settings,
                              list(superpose.polygon = list(col = fill)))

  if (isTRUE(auto.key) || is.list(auto.key)) {
    auto.key <- update_list(list(
      rectangles = TRUE, points = FALSE
    ), if (is.list(auto.key)) auto.key else list())
  } else {
    auto.key <- FALSE
  }

  setnames <- factor(rownames(dd))
  rownames(dd) <- NULL
  dd <- as.data.frame(dd)
  dd$set <- setnames

  if (is_by) {
    d <- dim(x)
    dn <- dimnames(x)
    n <- NROW(x[[1]]$coefficients)

    factors <- lapply(dn, as.factor)
    levels <- names(factors)
    factors <- expand.grid(factors)
    factors <- factors[rep(seq_len(NROW(factors)), each = n), , drop = FALSE]
    rownames(factors) <- NULL
    dd <- cbind(dd, factors)
  } else {
    levels <- NULL
  }

  # Retrieve call
  ccall <- match.call()
  ocall <- sys.call(sys.parent())
  ocall[[1]] <- quote(eulerplot)

  # Update call
  ccall$x <- stats::as.formula(
    paste("y ~ x",
          if (is_by) paste("|", paste(levels, collapse = " + ")) else "")
  )

  ccall$data <- dd
  if (!is.null(dd$r)) {
    ccall$ra  <- dd$r
    ccall$rb  <- dd$r
    ccall$phi <- rep.int(0, times = length(dd$r))
  } else {
    ccall$ra  <- dd$a
    ccall$rb  <- dd$b
    ccall$phi <- dd$phi
  }

  ccall$groups <- quote(set)
  ccall$panel <- panel
  ccall$default.prepanel <- default.prepanel
  ccall$counts <- counts
  ccall$aspect <- "iso"
  ccall$labels <- labels
  ccall$original.values <- orig
  ccall$fitted.values <- fitted
  ccall$fontface <- fontface
  ccall$default.scales <- default.scales
  ccall$par.settings <- par.settings
  ccall$xlab <- ""
  ccall$ylab <- ""
  ccall$auto.key <- auto.key
  ccall$fill <- fill

  # Make the call
  ccall[[1]] <- quote(lattice::xyplot)
  ans <- eval.parent(ccall)
  ans$call <- ocall
  ans
}

#' Prepanel Function for Euler Diagrams
#'
#' @inheritParams panel.euler
#' @param ... Ignored.
#'
#' @return A list of `xlim` and `ylim` items.
#' @export
prepanel.euler <- function(x,
                           y,
                           ra,
                           rb,
                           phi,
                           subscripts,
                           ...) {
  a   <- ra[subscripts]
  b   <- rb[subscripts]
  phi <- phi[subscripts]
  tx  <- atan2(-b*tan(phi), a)
  ty  <- atan2(b*tan(pi/2L - phi), a)

  list(xlim = range(x + a*cos(tx)*cos(phi) - b*sin(tx)*sin(phi),
                    x + a*cos(tx + pi)*cos(phi) - b*sin(tx + pi)*sin(phi)),
       ylim = range(y + b*sin(ty)*cos(phi) + a*cos(ty)*sin(phi),
                    y + b*sin(ty + pi)*cos(phi) + a*cos(ty + pi)*sin(phi)))
}

#' Panel Function for Euler Diagrams
#'
#' @param x X coordinates for the circle centers.
#' @param y Y coordinates for the circle centers.
#' @param ra Semi-major axes.
#' @param rb Semi-minor axes.
#' @param phi Rotation of the ellipse (as the counter-clockwise angle from
#'   the positive x-axis to the semi-major axis).
#' @param subscripts A vector of subscripts (See [lattice::xyplot()]).
#' @param fill Fill color for circles. (See [grid::gpar()].)
#' @param lty Line type for circles. (See [grid::gpar()].)
#' @param lwd Line weight for circles. (See [grid::gpar()].)
#' @param border Border color for circles.
#' @param alpha Alpha for circles. Note that [plot.euler()] by default
#'   modifies the alpha of `col` instead to avoid affecting the alpha of
#'   the borders. (See [grid::gpar()].)
#' @param fontface Fontface for the labels.  (See [grid::gpar()].)
#' @param counts Plots the original values for the disjoint set combinations
#'   (`original.values`). Can also be a list, in which the contents of the list
#'   will be passed on to [lattice::panel.text()] to modify the appearance of
#'   the counts.
#' @param labels Labels to plot on the circles.
#' @param original.values Original values for the disjoint set combinations.
#' @param fitted.values Fitted values for the disjoint set combinations.
#' @param ... Passed down to [panel.euler.circles()] and [panel.euler.labels()].

#'
#' @seealso [grid::gpar()].
#'
#' @return Plots euler diagrams inside a trellis panel.
#'
#' @export
panel.euler <- function(x,
                        y,
                        ra,
                        rb,
                        phi,
                        subscripts,
                        fill = superpose.polygon$col,
                        lty = superpose.polygon$lty,
                        lwd = superpose.polygon$lwd,
                        border = superpose.polygon$border,
                        alpha = superpose.polygon$alpha,
                        fontface = "bold",
                        counts = TRUE,
                        labels = NULL,
                        original.values,
                        fitted.values,
                        ...) {
  superpose.polygon <- lattice::trellis.par.get("superpose.polygon")

  assertthat::assert_that(assertthat::is.flag(counts) || is.list(counts))

  if (is.matrix(original.values)) {
    original.values <- original.values[, lattice::packet.number()]
    fitted.values   <-   fitted.values[, lattice::packet.number()]
  }

  # Plot circles if the semi-major and semi-minor axis are all equal.
  if (isTRUE(all.equal(ra, rb))) {
    panel.euler.circles(x = x,
                        y = y,
                        r = ra[subscripts],
                        fill = fill,
                        lty = lty,
                        lwd = lwd,
                        border = border,
                        identifier = "euler",
                        ...)
  } else {
    panel.euler.ellipses(x = x,
                         y = y,
                         ra = ra[subscripts],
                         rb = rb[subscripts],
                         phi = phi[subscripts],
                         fill = fill,
                         lty = lty,
                         lwd = lwd,
                         border = border,
                         identifier = "euler",
                         ...)
  }

  if ((is.list(counts) || isTRUE(counts)) || !is.null(labels)) {
    panel.euler.labels(x = x,
                       y = y,
                       ra = ra[subscripts],
                       rb = rb[subscripts],
                       phi = phi[subscripts],
                       labels = labels,
                       counts = counts,
                       original.values = original.values,
                       fitted.values = fitted.values,
                       fontface = fontface,
                       ...)
  }
}

#' Panel Function for Circles
#'
#' @inheritParams panel.euler
#' @param r Radius of the circle
#' @param border Border color.
#' @param fill Circle fill.
#' @param ... Passed on to [grid::grid.circle()].
#' @param col Ignored
#' @param font Ignored
#' @param fontface Ignored
#' @param identifier A character string that is prepended to the name of the
#'   grob that is created.
#' @param name.type A character value indicating whether the name of the grob
#'   should have panel or strip information added to it. Typically either
#'   `"panel"`, `"strip"`, `"strip.left"`, or `""` (for no extra information).
#'
#' @seealso [grid::grid.circle()].
#'
#' @return Plots circles inside a trellis panel.
#' @export
panel.euler.circles <- function(
  x,
  y,
  r,
  border = "black",
  fill = "transparent",
  ...,
  identifier = NULL,
  name.type = "panel",
  col,
  font,
  fontface
) {
  if (sum(!is.na(x)) < 1)
    return()

  border <- if (all(is.na(border)))
    "transparent"
  else if (is.logical(border))
    if (border) "black" else "transparent"
  else
    border

  if (hasGroupNumber())
    group <- list(...)$group.number
  else
    group <- 0

  xy <- grDevices::xy.coords(x, y, recycle = TRUE)
  grid::grid.circle(
    x = xy$x,
    y = xy$y,
    r = r,
    default.units = "native",
    gp = grid::gpar(fill = fill, col = border, ...),
    name = primName("circles", identifier, name.type, group)
  )
}

#' Panel Function for Ellipses
#'
#' @inheritParams panel.euler
#' @param border Border color.
#' @param fill Ellipse fill.
#' @param n Number of vertices to draw for each ellipse.
#' @param ... Passed on to [gridExtra::grid.ellipse()].
#' @param col Ignored
#' @param font Ignored
#' @param fontface Ignored
#' @param identifier A character string that is prepended to the name of the
#'   grob that is created.
#' @param name.type A character value indicating whether the name of the grob
#'   should have panel or strip information added to it. Typically either
#'   `"panel"`, `"strip"`, `"strip.left"`, or `""` (for no extra information).
#'
#' @seealso [gridExtra::grid.ellipse()].
#'
#' @return Plots ellipses inside a trellis panel.
#' @export
panel.euler.ellipses <- function(x,
                                 y,
                                 ra,
                                 rb,
                                 phi,
                                 border = "black",
                                 fill = "transparent",
                                 n = 200,
                                 ...,
                                 identifier = NULL,
                                 name.type = "panel",
                                 col,
                                 font,
                                 fontface) {
  if (sum(!is.na(x)) < 1)
    return()

  border <- if (all(is.na(border)))
    "transparent"
  else if (is.logical(border))
    if (border) "black" else "transparent"
  else
    border

  if (hasGroupNumber())
    group <- list(...)$group.number
  else
    group <- 0L

  N <- length(x)
  xy <- matrix(NA, nrow = n * N, ncol = 2L)

  for (i in seq_along(x)) {
    theta <- seq.int(0L, 2L * pi, length.out = n)

    j <- seq.int(((i - 1L)*n + 1L), i*n, 1L)

    xy[j, 1L] <-
      x[i] + ra[i]*cos(theta)*cos(phi[i]) - rb[i]*sin(theta)*sin(phi[i])
    xy[j, 2L] <-
      y[i] + rb[i]*sin(theta)*cos(phi[i]) + ra[i]*cos(theta)*sin(phi[i])

  }

  grid::grid.polygon(
    x = xy[, 1L],
    y = xy[, 2L],
    id.lengths = rep.int(n, N),
    default.units = "native",
    gp = grid::gpar(fill = fill, col = border),
    name = primName("circles", identifier, name.type, group)
  )
}

#' Panel Function for Circle Labels
#'
#' @inheritParams panel.euler
#' @param ... Arguments passed on to [panel.text()]
#'
#' @return Computes and plots labels or counts inside the centers of the
#'   circles' overlaps.
#' @export
panel.euler.labels <- function(
    x,
    y,
    ra,
    rb,
    phi,
    labels,
    counts = TRUE,
    original.values,
    fitted.values,
    ...
) {
  n <- length(x)
  id <- bit_indexr(n)
  singles <- rowSums(id) == 1L

  do_counts <- isTRUE(counts) || is.list(counts)
  do_labels <- !is.null(labels)

  centers <- locate_centers(x = x,
                            y = y,
                            a = ra,
                            b = rb,
                            phi = phi,
                            original.values = original.values,
                            fitted.values = fitted.values)

  # Plot counts
  if (do_counts) {
    do.call(lattice::panel.text, update_list(list(
      x = centers$x[singles],
      y = centers$y[singles],
      labels = centers$n[singles],
      identifier = "counts",
      offset = if (do_labels) 0.25 else NULL,
      pos = if (do_labels) 1 else NULL
    ), if (is.list(counts)) counts else list()))

    do.call(lattice::panel.text, update_list(list(
      x = centers$x[!singles],
      y = centers$y[!singles],
      labels = centers$n[!singles],
      identifier = "counts"
    ), if (is.list(counts)) counts else list()))
  }

  # Plot labels
  if (do_labels)
    do.call(lattice::panel.text, update_list(list(
      centers$x[singles],
      centers$y[singles],
      labels,
      offset = 0.25,
      pos = if (do_counts) 3L else NULL,
      identifier = "labels",
      name.type = "panel"
    ), list(...)))
}


#' Locate Centers of Circle Overlaps
#'
#' @inheritParams panel.euler
#'
#' @return A data frame with centers of the circle overlaps and their
#'   respective original counts.
#' @keywords internal
locate_centers <- function(x,
                           y,
                           a,
                           b,
                           phi,
                           original.values,
                           fitted.values) {
  n <- length(x)

  if (n > 1L) {
    n_samples <- 500L
    seqn    <- seq.int(0L, n_samples - 1L, 1L)
    theta   <- seqn * pi * (3L - sqrt(5L))
    rad     <- sqrt(seqn / n_samples)
    P       <- matrix(NA, ncol = n_samples, nrow = 3)
    P[1, ]  <- rad * cos(theta)
    P[2, ]  <- rad * sin(theta)
    P[3, ]  <- 1L

    id <- eulerr:::bit_indexr(n)
    n_combos <- nrow(id)

    # In case the user asks for counts, compute locations for these
    xx <- yy <- rep.int(NA_real_, nrow(id))

    not_zero <- fitted.values > .Machine$double.eps ^ 0.25

    singles <- rowSums(id) == 1L

    for (i in seq_along(x)) {
      PP <-
        translate(x[i], y[i]) %*% rotate(phi[i]) %*% scale(a[i], b[i]) %*% P

      in_which <- find_surrounding_sets(PP[1, ], PP[2, ], x, y, a, b, phi)

      for (j in seq_len(nrow(id))[id[, i]]) {
        idj <- id[j, ]
        if (all(is.na(xx[j]), idj[i])) {
          if (singles[j]) {
            sums <- colSums(in_which)
            locs <- sums == min(sums)
          } else {
            locs <- colSums(in_which == idj) == nrow(in_which)
          }

          if (any(locs)) {
            PP2 <- PP[, locs]

            mm <- matrix(NA, ncol = NCOL(PP2), nrow = length(x))
            for (k in seq_along(x)) {
              PP3 <- rotate(-phi[k]) %*% translate(-x[k], -y[k]) %*% PP2
              for (l in 1:NCOL(PP3)) {
                mm[k, l] <- dist_to_ellipse(a[k], b[k], PP3[1, l], PP3[2, l])
              }
            }
            labmax <- max_colmins(mm)
            xx[j] <- PP2[1, labmax]
            yy[j] <- PP2[2, labmax]
          }
        }
      }
    }
  } else {
    # One circle, always placed in the middle
    xx <- yy <- 0L
    singles <- TRUE
    n_combos <- 1L
  }

  data.frame(x = xx, y = yy, n = original.values)
}


rotate <- function(phi) {
  matrix(c(cos(phi), -sin(phi), 0,
           sin(phi),  cos(phi), 0,
           0,                0, 1),
         byrow = TRUE,
         ncol = 3)
}


translate <- function(x, y) {
  matrix(c(1, 0, x,
           0, 1, y,
           0, 0, 1),
         byrow = TRUE,
         ncol = 3)
}

scale <- function(x, y) {
  matrix(c(x, 0, 0,
           0, y, 0,
           0, 0, 1),
         byrow = TRUE,
         ncol = 3)
}
