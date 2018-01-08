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
#' @param quantities Plot quantities.
#' @param mode "`split`", the default, splits up the diagram into individual
#'   polygons and blends the colors of the overlapping shapes using
#'   color averaging in the CIELAB color space. "`overlay`" superposes
#'   sets and should be used in conjunction with a suitable `fill_alpha` value.
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
#'
#' @inherit lattice::levelplot return
#'
#' @export
#'
#' @seealso [panel.euler.circles()], [panel.euler.labels()],
#'   [panel.euler.ellipses()],
#'   [lattice::xyplot()], [grid::gpar()], [grid::grid.circle()],
#'   [lattice::panel.xyplot()], [euler()], [qualpalr::qualpal()]
#'
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
#'
#' # eulerr provides two modes for plotting the diagram
#' plot(fit, mode = "split")
#' plot(fit, mode = "overlay")
#'
#' # Customize colors, remove borders, bump alpha, color labels white
#' plot(fit,
#'      fill_alpha = 0.5,
#'      fill = c("red", "steelblue4"),
#'      col = "white",
#'      border = "transparent",
#'      fontface = "bold.italic")
#'
#' # Add quantities to the plot
#' plot(fit, quantities = TRUE)
#'
#' # Add a custom legend and retain quantities
#' plot(fit, quantities = TRUE, auto.key = list(space = "bottom", columns = 2))
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
plot.euler <- function(x,
                       fill = qualpalr_pal,
                       fill_alpha = 0.4,
                       auto.key = FALSE,
                       quantities = FALSE,
                       labels = is.logical(auto.key) && !isTRUE(auto.key),
                       fontface = "bold",
                       par.settings = list(),
                       ...,
                       default.prepanel = prepanel.euler,
                       default.scales = list(draw = FALSE),
                       panel = panel.euler,
                       mode = c("split", "overlay")) {
  stopifnot(is.numeric(fill_alpha),
            length(fill_alpha) == 1L,
            is.logical(auto.key) || is.list(auto.key),
            is.logical(quantities) || is.list(quantities),
            all(is.character(mode)))

  is_by <- inherits(x, "by")

  if (is.function(fill))
    fill <- fill(if (is_by) NROW(x[[1]]$coefficients) else NROW(x$coefficients))

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

  if (isTRUE(auto.key) || is.list(auto.key)) {
    auto.key <- update_list(list(
      rectangles = TRUE, points = FALSE
    ), if (is.list(auto.key)) auto.key else list())
  } else {
    auto.key <- FALSE
  }

  setnames <- as.factor(rownames(dd))
  rownames(dd) <- NULL
  dd <- as.data.frame(dd)
  dd$set <- setnames

  key_col <- grDevices::adjustcolor(fill, fill_alpha)
  par.settings <- update_list(
    par.settings,
    list(superpose.polygon = list(col = key_col[order(as.character(setnames))]))
  )

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
    paste("k ~ h",
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
  ccall$quantities <- quantities
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
  ccall$mode <- match.arg(mode)
  ccall$fill_alpha <- fill_alpha

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
prepanel.euler <- function(x, y, ra, rb, phi, subscripts, ...) {
  get_bounding_box(h = x, k = y, a = ra[subscripts], b = rb[subscripts],
                   phi = phi[subscripts])
}

#' Panel Function for Euler Diagrams
#'
#' Plots circular euler diagrams if `ra == rb` and elliptical such otherwise.
#'
#' @param x X coordinates for the centers.
#' @param y Y coordinates for the centers.
#' @param ra Semi-major axes.
#' @param rb Semi-minor axes.
#' @param phi Rotation of the ellipse (as the counter-clockwise angle from
#'   the positive x-axis to the semi-major axis).
#' @param subscripts A vector of subscripts (See [lattice::xyplot()]).
#' @param fill Fill color. (See [grid::gpar()].)
#' @param lty Line type. (See [grid::gpar()].)
#' @param lwd Line weight. (See [grid::gpar()].)
#' @param border Border color.
#' @param alpha Alpha (opacity) for the lines.
#' @param mode "`split`", the default, splits up the diagram into individual
#'   polygons and blends the colors of the overlapping shapes using
#'   color averaging in the CIELAB color space. "`overlay`" superposes
#'   sets and should be used in conjunction with a suitable `fill_alpha` value.
#' @param fill_alpha Alpha (opacity) for the fill. (See [grid::gpar()].)
#' @param fontface Fontface for the labels. (See [grid::gpar()].)
#' @param quantities Plots the original values for the disjoint set combinations
#'   (`original.values`). Can also be a list, in which the contents of the list
#'   will be passed on to [lattice::panel.text()] to modify the appearance of
#'   the quantity labels.
#' @param labels Labels.
#' @param original.values Original values for the disjoint set combinations.
#' @param fitted.values Fitted values for the disjoint set combinations.
#' @param ... Passed down to [panel.euler.ellipses()] and [panel.euler.labels()].
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
                        fill_alpha = 0.4,
                        fontface = "bold",
                        quantities = FALSE,
                        labels = NULL,
                        original.values,
                        fitted.values,
                        mode = c("split", "overlay"),
                        ...) {
  stopifnot(is.logical(quantities) || is.list(quantities))

  superpose.polygon <- lattice::trellis.par.get("superpose.polygon")

  if (is.matrix(original.values)) {
    original.values <- original.values[, lattice::packet.number()]
    fitted.values   <-   fitted.values[, lattice::packet.number()]
  }

  # Plot circles if the semi-major and semi-minor axis are all equal.
  panel.euler.ellipses(x = x,
                       y = y,
                       ra = ra[subscripts],
                       rb = rb[subscripts],
                       phi = phi[subscripts],
                       fill = fill,
                       fill_alpha = fill_alpha,
                       alpha = alpha,
                       lty = lty,
                       lwd = lwd,
                       border = border,
                       mode = mode,
                       identifier = "euler",
                       fitted.values = fitted.values,
                       ...)

  if ((is.list(quantities) || isTRUE(quantities)) || !is.null(labels)) {
    panel.euler.labels(x = x,
                       y = y,
                       ra = ra[subscripts],
                       rb = rb[subscripts],
                       phi = phi[subscripts],
                       labels = labels,
                       quantities = quantities,
                       original.values = original.values,
                       fitted.values = fitted.values,
                       fontface = fontface,
                       identifier = "euler",
                       ...)
  }
}

#' Panel Function for Euler Circles (deprecated)
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
panel.euler.circles <- function(x,
                                y,
                                r,
                                border = "black",
                                fill = "transparent",
                                fill_alpha = 0.4,
                                ...,
                                identifier = NULL,
                                name.type = "panel",
                                col,
                                font,
                                fontface) {
  .Deprecated("panel.euler.ellipses")
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

  xy <- grDevices::xy.coords(x, y, recycle = TRUE)
  grid::grid.circle(
    x = xy$x,
    y = xy$y,
    r = r,
    default.units = "native",
    gp = grid::gpar(fill = grDevices::adjustcolor(fill, fill_alpha),
                    col = border, ...),
    name = primName("circles", identifier, name.type, group)
  )
}

#' Panel Function for Euler Ellipses
#'
#' @inheritParams panel.euler
#' @param border Border color.
#' @param fill Ellipse fill.
#' @param n Number of vertices to draw for each ellipse.
#' @param ... Passed on to [grid::grid.polygon()].
#' @param col Ignored
#' @param font Ignored
#' @param fontface Ignored
#' @param identifier A character string that is prepended to the name of the
#'   grob that is created.
#' @param name.type A character value indicating whether the name of the grob
#'   should have panel or strip information added to it. Typically either
#'   `"panel"`, `"strip"`, `"strip.left"`, or `""` (for no extra information).
#'
#' @seealso [grid::grid.polygon()].
#'
#' @return Plots ellipses inside a trellis panel.
#' @export
panel.euler.ellipses <- function(x,
                                 y,
                                 ra,
                                 rb,
                                 phi,
                                 fill = "transparent",
                                 fill_alpha = 0.4,
                                 border = "black",
                                 mode = c("split", "overlay"),
                                 identifier = NULL,
                                 n = 200,
                                 fitted.values,
                                 ...,
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
  e <- ellipse(x, y, ra, rb, phi, n = n)

  if (mode == "overlay" || identical(N, 1L)) {
    grid::grid.polygon(
      x = c(lapply(e, "[[", "x"), recursive = TRUE),
      y = c(lapply(e, "[[", "y"), recursive = TRUE),
      id.lengths = rep.int(n, N),
      default.units = "native",
      gp = grid::gpar(fill = grDevices::adjustcolor(fill, fill_alpha),
                      col = border),
      name = primName("ellipse", identifier, name.type, group)
    )
  } else {
    id <- bit_indexr(N)
    m <- NROW(id)

    pieces <- vector("list", m)
    for (i in rev(seq_len(m))) {
      idx <- which(id[i, ])
      n_idx <- length(idx)
      if (n_idx == 1L) {
        pieces[[i]] <- e[[idx[1]]]
      } else {
        pieces[[i]] <- poly_clip(e[[idx[1L]]], e[[idx[2L]]], "intersection")
        if (n_idx > 2L) {
          for (j in 3L:n_idx) {
            pieces[[i]] <- poly_clip(pieces[[i]], e[[idx[j]]], "intersection")
          }
        }
      }
      for (k in which(!id[i, ])) {
        pieces[[i]] <- poly_clip(pieces[[i]], e[[k]], "minus")
      }
      if (i > N) {
        fill[i] <- mix_colors(fill[idx])
      }
    }

    # honor users' color choices if appropriate length
    if (length(fill) != m && length(fill) >= N) {
      fill <- fill[seq_len(N)]
    }
    fill <- grDevices::adjustcolor(fill, fill_alpha)

    for (i in seq_along(pieces)) {
      if (is.null(pieces[[i]]$x)) {
        x0 <- lapply(pieces[[i]], "[[", "x")
        y0 <- lapply(pieces[[i]], "[[", "y")
        len <- lengths(x0)
      } else {
        x0 <- pieces[[i]]$x
        y0 <- pieces[[i]]$y
        len <- length(x0)
      }

      if (length(x0) > 0) {
        grid::grid.path(
          x = c(x0, recursive = TRUE),
          y = c(y0, recursive = TRUE),
          default.units = "native",
          id.lengths = len,
          gp = grid::gpar(fill = fill[i], col = "transparent"),
          name = primName("subset", identifier, name.type, group)
        )
      }
    }

    grid::grid.polygon(
      x = c(lapply(e, "[[", "x"), recursive = TRUE),
      y = c(lapply(e, "[[", "y"), recursive = TRUE),
      id.lengths = rep.int(n, N),
      default.units = "native",
      gp = grid::gpar(fill = "transparent", col = border),
      name = primName("ellipse", identifier, name.type, group)
    )
  }
}

#' Panel Function for Euler Diagram Labels
#'
#' @inheritParams panel.euler
#' @param ... Arguments passed on to [panel.text()]
#' @return Computes and plots labels or quantities inside the centers of the
#'   ellipses' overlaps.
#' @export
panel.euler.labels <- function(x,
                               y,
                               ra,
                               rb,
                               phi,
                               labels,
                               quantities = FALSE,
                               original.values,
                               fitted.values,
                               ...) {
  n <- length(x)
  id <- bit_indexr(n)
  singles <- rowSums(id) == 1
  empty <- abs(fitted.values) < sqrt(.Machine$double.eps)

  do_quantities <- isTRUE(quantities) || is.list(quantities)
  do_labels <- !is.null(labels)

  centers <- locate_centers(h = x,
                            k = y,
                            a = ra,
                            b = rb,
                            phi = phi,
                            fitted = fitted.values)

  centers <- t(centers)
  centers <- cbind(centers, original.values)

  center_labels <- labels[!is.nan(centers[singles, 1L])]
  label_centers <- centers[!is.nan(centers[, 1L]) & singles, , drop = FALSE]

  droprows <- rep.int(TRUE, NROW(centers))
  for (i in which(is.nan(centers[singles, 1L]))) {
    pick <- id[, i] & !empty & !is.nan(centers[, 1L])
    label_centers <- rbind(label_centers, centers[which(pick)[1L], ])
    center_labels <- c(center_labels, labels[i])
    droprows[which(pick)[1]] <- FALSE
  }

  count_centers <-
    centers[!is.nan(centers[, 1L]) & !singles & droprows, , drop = FALSE]

  # Plot quantities
  if (do_quantities) {
    do.call(lattice::panel.text, update_list(list(
      x = label_centers[, 1L],
      y = label_centers[, 2L],
      labels = label_centers[, 3L],
      identifier = "quantities",
      offset = if (do_labels) 0.25 else NULL,
      pos = if (do_labels) 1L else NULL
    ), if (is.list(quantities)) quantities else list()))

    do.call(lattice::panel.text, update_list(list(
      x = count_centers[, 1L],
      y = count_centers[, 2L],
      labels = count_centers[, 3L],
      identifier = "quantities"
    ), if (is.list(quantities)) quantities else list()))
  }

  # Plot labels
  if (do_labels)
    do.call(lattice::panel.text, update_list(list(
      x = label_centers[, 1L],
      y = label_centers[, 2L],
      labels = center_labels,
      offset = 0.25,
      pos = if (do_quantities) 3L else NULL,
      identifier = "labels",
      name.type = "panel"
    ), list(...)))
}
