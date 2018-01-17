#' Prepanel Function for Euler Diagrams (deprecated)
#'
#' @inheritParams panel.euler
#' @param ... Ignored.
#'
#' @return A list of `xlim` and `ylim` items.
#' @export
prepanel.euler <- function(x, y, ra, rb, phi, subscripts, ...) {
  .Deprecated("plot.euler")
  get_bounding_box(h = x, k = y, a = ra[subscripts], b = rb[subscripts],
                   phi = phi[subscripts])
}

#' Panel Function for Euler Diagrams (deprecated)
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
  .Deprecated("plot.euler")
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
  .Deprecated("plot.euler")
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

#' Panel Function for Euler Ellipses (deprecated)
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
  .Deprecated("plot.euler")
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

  grob_fills <- grid::gList()

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

#' Panel Function for Euler Diagram Labels (deprecated)
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
  .Deprecated("plot.euler")
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
