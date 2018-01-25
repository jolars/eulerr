# eulerr: Area-Proportional Euler and Venn Diagrams with Circles or Ellipses
# Copyright (C) 2018 Johan Larsson <johanlarsson@outlook.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Deprecated functions in package \pkg{eulerr}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name eulerr-deprecated
#' @keywords internal
NULL

#' Prepanel Function for Euler Diagrams (deprecated)
#'
#' @inheritParams panel.euler
#' @param x X coordinates for the centers.
#' @param y Y coordinates for the centers.
#' @param ra Semi-major axes.
#' @param rb Semi-minor axes.
#' @param phi Rotation of the ellipse (as the counter-clockwise angle from
#'   the positive x-axis to the semi-major axis).
#'
#' @return A list of `xlim` and `ylim` items.
#'
#' @name prepanel.euler-deprecated
#' @usage prepanel.euler(x, y, ra, rb, phi, subscripts, ...)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `prepanel.euler`:
#' For `prepanel.euler()`, use [plot.euler()].
#'
#' @export
prepanel.euler <- function(x,
                           y,
                           ra,
                           rb,
                           phi,
                           subscripts,
                           ...) {
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
#' @name panel.euler-deprecated
#' @usage panel.euler(x, y, ra, rb, phi, subscripts, fill,
#'   lty, lwd, border, alpha, fill_alpha, fontface, quantities,
#'   labels, original.values,  fitted.values, mode, ...)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `panel.euler`:
#' For `panel.euler()`, use [plot.euler()].
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
#' @param x X coordinates for the centers.
#' @param y Y coordinates for the centers.
#' @param r Radius of the circle
#' @param border Border color.
#' @param fill Circle fill.
#' @param fill_alpha Fill opacity
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
#'
#' @name panel.euler.circles-deprecated
#' @usage panel.euler.circles(x, y, r, border, fill, fill_alpha, ...,
#'   identifier, name.type, col, font, fontface)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `panel.euler.circles`:
#' For `panel.euler.circles()`, use [plot.euler()].
#'
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
#' @param x X coordinates for the centers.
#' @param y Y coordinates for the centers.
#' @param ra Semi-major axes.
#' @param rb Semi-minor axes.
#' @param phi Rotation of the ellipse (as the counter-clockwise angle from
#'   the positive x-axis to the semi-major axis).
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
#' @param mode "`split`", the default, splits up the diagram into individual
#'   polygons and blends the colors of the overlapping shapes using
#'   color averaging in the CIELAB color space. "`overlay`" superposes
#'   sets and should be used in conjunction with a suitable `fill_alpha` value.
#' @param original.values Original values for the disjoint set combinations.
#' @param fitted.values Fitted values for the disjoint set combinations.
#'
#' @seealso [grid::grid.polygon()].
#'
#' @return Plots ellipses inside a trellis panel.
#'
#' @name panel.euler.ellipses-deprecated
#' @usage panel.euler.ellipses(x, y, ra, rb, phi, fill = "transparent",
#'   fill_alpha = 0.4, border = "black", mode = c("split", "overlay"),
#'   identifier = NULL, n = 200, fitted.values, ..., name.type = "panel",
#'   col, font, fontface)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `panel.euler.ellipses`:
#' For `panel.euler.ellipses()`, use [plot.euler()].
#'
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
#' @inheritParams panel.euler-deprecated
#' @param ... Arguments passed on to [panel.text()]
#' @return Computes and plots labels or quantities inside the centers of the
#'   ellipses' overlaps.
#'
#' @name panel.euler.labels-deprecated
#' @usage panel.euler.labels(x, y, ra, rb, phi, labels, quantities, original.values,
#'   fitted.values, ...)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `panel.euler.labels`:
#' For `panel.euler.labels()`, use [plot.euler()].
#'
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

#' Compute Locations for Overlaps (deprecated)
#'
#' Runs the same algorithm as in [plot.euler()] / [panel.euler.labels()] to
#' label the overlaps in the Euler diagram. This is useful if you want to
#' use your own solution to plot the final diagram.
#'
#' In the cases where `x` is a list of Euler diagrams (if the `by` argument
#' was used in the call to [euler()]), this function returns a list of
#' matrices with coordinates.
#'
#' @param x An object of class 'euler'
#' @param labels An optional character vector of labels for the diagram.
#'
#' @return A numeric matrix of x and y coordinates for the labels, as well as
#'   the quantities and proportions for the overlaps depicted in the labels.
#'
#' @name label-deprecated
#' @usage label(x, labels)
#' @seealso [eulerr-deprecated]
#' @keywords internal
NULL

#' @rdname eulerr-deprecated
#' @section `label.euler`:
#' For `label.euler()`, use [plot.euler()].
#'
#' @export
label <- function(x, labels = NULL) UseMethod("label")

#' @rdname eulerr-deprecated
#' @export
label.euler <- function(x, labels = NULL) {
  .Deprecated("plot.euler")
  if (inherits(x, "by")) {
    out <- lapply(x, label)

    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    gg <- lapply(seq_along(x), function(i, x) {
      ii <- i - 1L
      nms <- character(length(dn))
      for (j in seq_along(dn)) {
        iii <- ii%%d[j] + 1L
        ii <- ii%/%d[j]

        nms[j] <- dn[[j]][iii]
      }
      paste(nms, collapse = "_")
    }, x)
    names(out) <- unlist(gg)

    out
  } else {
    coefs <- x$coefficients
    fitted.values <- x$fitted.values
    quantity <- x$original.values

    h <- coefs[, 1L]
    k <- coefs[, 2L]

    if (ncol(coefs) == 3L) {
      # Circles
      a <- b <- coefs[, 3L]
      phi <- rep.int(0, length(a))
    } else {
      # Ellipses
      a <- coefs[, 3L]
      b <- coefs[, 4L]
      phi <- coefs[, 5L]
    }

    n <- length(h)
    id <- bit_indexr(n)
    singles <- rowSums(id) == 1
    empty <- abs(fitted.values) < sqrt(.Machine$double.eps)

    if (is.null(labels))
      labels <- names(quantity)

    stopifnot(length(labels) == nrow(id))

    centers <- locate_centers(h = h,
                              k = k,
                              a = a,
                              b = b,
                              phi = phi,
                              fitted = fitted.values)

    centers <- t(centers)
    dimnames(centers) <- list(labels, c("x", "y"))

    centers <- cbind(centers, quantity)

    out <- stats::na.omit(centers)
    cbind(out, proportion = out[, 3L]/sum(quantity))
  }
}

# The following functions have been imported from lattice version 0.20-35, which
# came with the following copyright notice attached.

### Copyright (C) 2001-2006  Deepayan Sarkar <Deepayan.Sarkar@R-project.org>
###
### This file is part of the lattice package for R.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
### MA 02110-1301, USA

primName <- function(name, identifier = NULL, name.type = "panel", group = 0) {
  lattice::trellis.grobname(name = ifelse(is.null(identifier),
                                          name,
                                          paste(identifier, name, sep = ".")),
                            type = name.type,
                            group = group)
}

hasGroupNumber <- function() {
  aname <- "group.number"
  fnames <- names(formals(sys.function(sys.parent())))
  if (is.na(match(aname, fnames))) {
    if (is.na(match("...", fnames)))
      FALSE
    else {
      dotsCall <- eval(quote(substitute(list(...))), sys.parent())
      !is.na(match(aname, names(dotsCall)))
    }
  } else FALSE
}


