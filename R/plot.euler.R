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

#' Plot Area-Proportional Euler Diagrams
#'
#' Plot Euler diagrams fit with [euler()] using [grid::Grid()] graphics. This
#' function sets up all the necessary plot paramters and computes
#' the geometry of the diagram. [print.euler_diagram()], meanwhile,
#' does the actual plotting of the diagram.
#'
#' Most of the arguments to this function accept either a logical, a vector, or
#' a list where
#'
#' * logical values set the attribute on or off,
#' * vectors are shortcuts to commonly used options (see the individual
#' parameters), and
#' * lists enable fine-grained control, including graphical
#' parameters as described in [grid::gpar()] and control
#' arguments that are specific to each argument.
#'
#' If the diagram has been fit using the `data.frame` or `matrix` methods
#' and using the `by` argument, the plot area will be split into panels for
#' each combination of the one to two factors.
#'
#' For users who are looking to plot their diagram using another package,
#' all the necessary parameters can be collected if the result of this
#' function is assigned to a variable (rather than printed to screen).
#'
#' @param x an object of class `'euler'`, generated from [euler()]
#' @param mode "`split`", the default, splits up the diagram into individual
#'   polygons and blends the colors of the overlapping shapes using
#'   color averaging in the CIELAB color space. "`overlay`" superposes
#'   sets and should be used in conjunction with a suitable `fill_alpha` value.
#' @param legend draw a legend using [grid::grid.legend()]. If a list,
#'   the item `side` can be used to set the location of the legend.
#' @param labels draw labels. Vectors are assumed
#'   to be a character or an expression vector. If a list, values `labels`,
#'   `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `fontface`,
#'   `lineheight`, and `font` are accepted. Labels are drawn using
#'   [grid::grid.text()].
#' @param quantities draw quantities. If a vector, the values in it will replace
#'   those of the default. If a list, values `labels`,
#'   `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `fontface`,
#'   `lineheight`, and `font` are accepted. Quantities are drawn using
#'   [grid::grid.text()].
#' @param edges draw edges. Vectors are assumed to be
#'   colors for edges of the ellipses. If a list, values
#'   `col`, `alpha`, `lty`, `lwd`, `lex`, `lineend`, `linejoin` and `linemitre`
#'   are accepted. Edges are drawn using [grid::grid.polygon()] if
#'   `mode == 'overlay'` and [grid::grid.polyline()] if `mode ==  'split'`.
#' @param fills draw fills. Character or integer vectors are assumed to be
#'   colors to fill the shapes in the diagram. If a list, values
#'   `fill` and `alpha` are accepted. If `mode == 'split'`, colors of
#'   intersecting overlaps will be mixed. Fills are drawn using
#'   [grid::grid.polygon()].
#' @param n number of vertices for the ellipses
#' @param strips a list of options for the strips, which are drawn if
#'   `x` is a list of `euler` objects and ignored otherwise. Arguments
#'   `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `fontface`,
#'   `lineheight`, and `font` are accepted.
#' @param ... ignored
#' @param fill deprecated
#' @param fill_alpha deprecated
#' @param auto.key deprecated
#' @param fontface deprecated
#' @param par.settings deprecated
#' @param default.prepanel deprecated
#' @param default.scales deprecated
#' @param panel deprecated
#'
#' @seealso [euler()], [print.euler_diagram()], [grid::gpar()],
#'   [grid::grid.polygon()], [grid::grid.polyline()],
#'   [grid::grid.legend()], [grid::grid.text()]
#'
#' @return Provides an `'euler_diagram'` object, which is a description of
#'   the diagram to be drawn. [print.euler_diagram()] does the actual plotting
#'   of the diagram and is usually called automatically after this function
#'   is called.
#' @export
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
#'      fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
#'      edges = FALSE,
#'      labels = list(col = "white", font = 4))
#'
#' # Add quantities to the plot
#' plot(fit, quantities = TRUE)
#'
#' # Add a custom legend and retain quantities
#' plot(fit, quantities = TRUE, legend = list(labels = c("foo", "bar")))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit, fills = FALSE, edges = list(lty = 1:2))
#'
#' # save plot parameters to plot using some other method
#' diagram_description <- plot(fit)
plot.euler <- function(x,
                       mode = c("split", "overlay"),
                       legend = FALSE,
                       fills = TRUE,
                       edges = TRUE,
                       labels = identical(legend, FALSE),
                       quantities = FALSE,
                       n = 200,
                       strips = NULL,
                       ...,
                       fill,
                       fill_alpha,
                       auto.key,
                       fontface,
                       par.settings,
                       default.prepanel,
                       default.scales,
                       panel) {
  mode <- match.arg(mode)

  stopifnot(n > 0,
            is.numeric(n) && length(n) == 1)

  if (!missing(fill)) {
    warning("'fill' is deprecated; please use 'fills' instead")
    fills <- update_list(fills, list(col = fill))
  }

  if (!missing(fill_alpha)) {
    warning("'fill_alpha' is deprecated; please use 'fills' instead")
    fills <- update_list(fills, list(alpha = fill_alpha))
  }

  if (!missing(auto.key)) {
    warning("'auto.key' is deprecated; please use 'legend'")
    legend <- isTRUE(auto.key)
  }

  if (!missing(fontface)) {
    warning("'fontface' is deprecated; please use 'labels'")
    if (is.list(labels))
      labels <- update_list(labels, list(fontface = fontface))
    else if (isTRUE(labels))
      labels <- list(fontface = fontface)
  }

  if (!missing(par.settings))
    warning("'par.settings' is deprecated")
  if (!missing(default.prepanel))
    warning("'default.prepanel' is deprecated")
  if (!missing(default.scales))
    warning("'default.scales' is deprecated")
  if (!missing(panel))
    warning("'panel' is deprecated")

  groups <- attr(x, "groups")
  do_groups <- !is.null(groups)
  do_labels <- is.list(labels) || isTRUE(labels) || is.character(labels) || is.expression(labels)
  do_quantities <- is.list(quantities) || isTRUE(labels) || is.character(quantities) || is.expression(quantities)
  do_edges <- is.list(edges) || isTRUE(edges) || is.numeric(edges) || is.character(edges)
  do_fills <- is.list(fills) || isTRUE(fills) || is.numeric(edges) || is.character(edges)
  do_legend <- is.list(legend) || isTRUE(legend)
  do_strips <- !is.null(groups)

  args <- list(x = x, do_fills = do_fills, do_edges = do_edges,
               do_labels = do_labels, do_quantities = do_quantieis,
               mode = mode, n = n)

  xx <- setup_geometry(x, do_fills, do_edges, do_labels, do_quantities, mode, n)

  # recurse if list
  if (do_groups) {
    out <- lapply(x, do_fills, do_edges, do_labels, do_quantities, mode, n)
    # setup strips
    if (!is.list(strips))
      strips <- TRUE

    n_levels <- sum(vapply(groups, function(y) max(as.numeric(y)),
                           FUN.VALUE = numeric(1)))

    strips <- setup_gpar(strips,
                         n = n_levels,
                         optional = list(col = "black",
                                         cex = 1,
                                         fontsize = 12,
                                         lineheight= 1.2,
                                         font = 4,
                                         fontfamily = "",
                                         alpha = 1,
                                         groups = as.list(groups)))

    attr(out, "strips") <- strips
    attr(out, "groups") <- groups
    class(out) <- c("euler_diagram")
    return(out)
  }

  dd <- x$coefficients
  orig <- x$original.values
  fitted <- x$fitted.values

  setnames <- rownames(dd)

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- length(h)
  n_id <- 2^n_e - 1
  id <- bit_indexr(n_e)

  e <- ellipse(h, k, a, b, phi, n = n)
  e_x <- c(lapply(e, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(e, "[[", "y"), recursive = TRUE)

  limits <- get_bounding_box(h, k, a, b, phi)

  if (is.list(fills))
    if (is.function(fills$fill))
      fills$fill <- fills$fill(n_e)

  # setup edges
  edges <- setup_gpar(edges,
                      n = n_e,
                      optional = list(),
                      required = list(fill = "transparent"))
  if (is.list(edges))
    edges$data <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))

  # setup fills
  if (is.list(fills))
    if (is.function(fills$gp$fill))
      fills$gp$fill <- fills$gp$fill(ifelse(mode == "overlay", n_e, n_id))

  fills <- setup_gpar(fills,
                      n = if (mode == "overlay") n_e else n_id,
                      optional = list(fill = qualpalr_pal(n_e), alpha = 0.4),
                      required = list(col = "transparent"))

  if (is.list(fills)) {
    # overlay ellipses on top of each other
    if (mode == "overlay" || n_e == 1) {
      fills <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))
    } else {
      # split fills into smaller polygons and mix colors
      fills$gp$fill <- rep_len(fills$gp$fill, n_id)
      pieces <- fills$data <- vector("list", n_id)
      for (i in rev(seq_len(n_id))) {
        idx <- which(id[i, ])
        n_idx <- length(idx)
        if (n_idx == 1L) {
          pieces[[i]] <- e[[idx[1]]]
        } else {
          pieces[[i]] <- eulerr:::poly_clip(e[[idx[1L]]], e[[idx[2L]]], "intersection")
          if (n_idx > 2L) {
            for (j in 3L:n_idx) {
              pieces[[i]] <- eulerr:::poly_clip(pieces[[i]], e[[idx[j]]], "intersection")
            }
          }
        }
        for (ii in which(!id[i, ])) {
          pieces[[i]] <- eulerr:::poly_clip(pieces[[i]], e[[ii]], "minus")
        }
        if (i > n_e) {
          fills$gp$fill[i] <- mix_colors(fills$gp$fill[idx])
        }
      }

      for (i in seq_along(pieces)) {
        if (is.null(pieces[[i]]$x)) {
          x0 <- lapply(pieces[[i]], "[[", "x")
          y0 <- lapply(pieces[[i]], "[[", "y")
        } else {
          x0 <- pieces[[i]]$x
          y0 <- pieces[[i]]$y
        }
        if (length(x0) > 0L) {
          fills$data[[i]]$x <- c(x0, recursive = TRUE)
          fills$data[[i]]$y <- c(y0, recursive = TRUE)
          fills$data[[i]]$id.lengths <- lengths(x0)
        }
      }
      not_empty <- !vapply(fills$data, is.null, logical(1))
      fills$data <- fills$data[not_empty]
      fills$gp$fill <- fills$gp$fill[not_empty]
      fills$gp$alpha <- fills$gp$alpha[not_empty]
    }
  }

  # setup quantities
  if (is.character(quantities) || is.expression(quantities))
    quantities <- list(quantities = quantities)
  quantities <- setup_gpar(quantities,
                           n = n_id,
                           optional = list(col = "black",
                                           cex = 1,
                                           fontsize = 12,
                                           lineheight= 1.2,
                                           font = 1,
                                           fontfamily = "",
                                           alpha = 1))

  # setup labels
  if (is.character(labels) || is.expression(labels))
    labels <- list(labels = labels)
  labels <- setup_gpar(labels,
                       n = n_e,
                       optional = list(font = 2))
  if (is.list(labels))
    if (is.null(labels$labels))
      labels$labels <- setnames

  if (is.list(labels) || is.list(quantities)) {
    singles <- rowSums(id) == 1
    empty <- abs(fitted) < sqrt(.Machine$double.eps)

    centers <- cbind(t(locate_centers(h, k, a, b, phi, fitted)), orig)
    if (is.list(labels))
      center_labels <- labels$labels[!is.nan(centers[singles, 1L])]
    labels_centers <- centers[!is.nan(centers[, 1L]) & singles, , drop = FALSE]

    droprows <- rep.int(TRUE, NROW(centers))
    for (i in which(is.nan(centers[singles, 1L]))) {
      pick <- id[, i] & !empty & !is.nan(centers[, 1L])
      labels_centers <- rbind(labels_centers, centers[which(pick)[1L], ])
      if (is.list(labels))
        center_labels <- c(center_labels, labels$labels[i])
      droprows[which(pick)[1]] <- FALSE
    }

    if (is.list(quantities)) {
      quantities_centers <-
        centers[!is.nan(centers[, 1L]) & !singles & droprows, , drop = FALSE]
      quantities_centers <- rbind(quantities_centers, labels_centers)
      quantities$data <- list(x = quantities_centers[, 1L],
                              y = quantities_centers[, 2L])
      if (is.null(quantities$labels))
        quantities$labels <- quantities_centers[, 3L]
    }

    if (is.list(labels)) {
      labels$data <- list(x = labels_centers[, 1L],
                          y = labels_centers[, 2L])
      labels$labels <- center_labels
    }
  }

  # setup legend
  legend <- setup_gpar(legend,
                       n = n_e,
                       optional = list(fill = mapply(grDevices::adjustcolor,
                                                     col = fills$gp$fill,
                                                     alpha.f = fills$gp$alpha),
                                       cex = 1.2,
                                       fontsize = 12/1.2,
                                       side = "right",
                                       col = mapply(grDevices::adjustcolor,
                                                    col = edges$gp$col,
                                                    alpha.f = edges$gp$alpha)))
  if (is.list(legend)) {
    if (is.null(legend$labels)) {
      if (is.list(labels)) {
        if (!is.null(labels$labels)) {
          legend$labels <- labels$labels
        }
      } else {
        legend$labels <- setnames
      }
    }
    if (is.null(legend$pch)) legend$pch <- 21
    if (is.null(legend$vgap)) legend$vgap <- grid::unit(0.25, "lines")
    legend$do.lines <- FALSE
  }

  structure(list(ellipses = dd,
                 mode = mode,
                 legend = legend,
                 fills = fills,
                 edges = edges,
                 labels = labels,
                 quantities = quantities,
                 original.values = orig,
                 fitted.values = fitted,
                 limits = limits),
            class = "euler_diagram")
}
#' Grobify Euler objects
#'
#' @param x an `euler` object
#'
#' @return A [grid::gList()] is returned.
#' @keywords internal
#'
grobify <- function(x) {
  if (!is.null(attr(x, "groups"))) {
    out <- lapply(x[!(names(x) %in% "strips")], grobify)
  } else {
    ellipses <- x$ellipses
    mode <- x$mode
    legend <- x$legend
    labels <- x$labels
    edges <- x$edges
    fills <- x$fills
    quantities <- x$quantities

    n_e <- nrow(ellipses)
    n_id <- 2L^n_e - 1L
    id <- bit_indexr(n_e)

    #edges
    if (is.list(edges)) {
      # edges
      edges_grob <- grid::polylineGrob(edges$data$x,
                                       edges$data$y,
                                       id.lengths = edges$data$id.lengths,
                                       default.units = "native",
                                       gp = do.call(grid::gpar, edges$gp))
    } else {
      edges_grob <- grid::nullGrob()
    }

    # fills
    if (is.list(fills)) {
      if (mode == "overlay" || n_e == 1L) {
        fills_grob <- grid::polygonGrob(fills$data$x,
                                        fills$data$y,
                                        id.lengths = fills$data$id.lengths,
                                        default.units = "native",
                                        gp = do.call(grid::gpar, fills$gp))

      } else {
        fills_grob <- list()
        for (i in seq_along(fills$data)) {
          fills_grob[[i]] <- grid::pathGrob(
            fills$data[[i]]$x,
            fills$data[[i]]$y,
            id.lengths = fills$data[[i]]$id.lengths,
            default.units = "native",
            gp = grid::gpar(fill = fills$gp$fill[i],
                            alpha = fills$gp$alpha[i],
                            col = "transparent")
          )
        }
        fills_grob <- do.call(grid::gList, fills_grob)
      }
    } else {
      fills_grob <- grid::nullGrob()
    }

    if (is.list(quantities))  {
      quantities_id <- rep.int(FALSE, length(quantities$data$x))
      if (is.list(labels))
        quantities_id[names(quantities$data$x) %in% names(labels$data$x)] <- TRUE
      lab_id <- quantities_id
    }

    # quantities
    if (is.list(quantities)) {
      quantities_grob <- grid::textGrob(
        label = quantities$labels,
        x = quantities$data$x,
        y = quantities$data$y,
        vjust = ifelse(lab_id & is.list(labels), 1, 0.5),
        name = "quantities",
        default.units = "native",
        gp = do.call(grid::gpar, quantities$gp)
      )
    } else {
      quantities_grob <- grid::nullGrob()
    }

    # labels
    if (is.list(labels)) {
      labels_grob <- grid::textGrob(
        label = labels$labels,
        x = labels$data$x,
        y = labels$data$y,
        vjust = if (is.list(quantities)) -0.5 else 0.5,
        name = "labels",
        default.units = "native",
        gp = do.call(grid::gpar, labels$gp)
      )
    } else {
      labels_grob <- grid::nullGrob()
    }

    out <- grid::grobTree(fills_grob, edges_grob, labels_grob, quantities_grob)
  }

  if (inherits(out, "gTree"))
    out <- list(out)

  do.call(grid::gList, out)
}

#' Compute geometries and label locations
#'
#' @param x an object of class 'euler'
#' @param do_fills do fills?
#' @param do_edges do edges?
#' @param do_labels do labels?
#' @param do_quantities do quantities?
#' @param mode mode
#'
#' @return a list object with slots for the various objects
#' @export
#' @keywords internal
setup_geometry <- function(x,
                           do_fills,
                           do_edges,
                           do_labels,
                           do_quantities,
                           mode,
                           n) {
  dd <- x$coefficients
  orig <- x$original.values
  fitted <- x$fitted.values

  #setnames <- rownames(dd)

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- NROW(dd)
  n_id <- 2^n_e - 1
  id <- eulerr:::bit_indexr(n_e)

  e <- eulerr:::ellipse(h, k, a, b, phi, n)
  e_x <- c(lapply(e, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(e, "[[", "y"), recursive = TRUE)

  limits <- eulerr:::get_bounding_box(h, k, a, b, phi)

  # setup edges
  if (do_edges)
    edges <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))

  if (do_fills) {
    # overlay ellipses on top of each other
    if (mode == "overlay" || n_e == 1) {
      fills <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))
    } else {
      pieces <- fills <- vector("list", n_id)
      for (i in rev(seq_len(n_id))) {
        idx <- which(id[i, ])
        n_idx <- length(idx)
        if (n_idx == 1L) {
          pieces[[i]] <- e[[idx[1]]]
        } else {
          pieces[[i]] <- eulerr:::poly_clip(e[[idx[1L]]], e[[idx[2L]]], "intersection")
          if (n_idx > 2L) {
            for (j in 3L:n_idx) {
              pieces[[i]] <- eulerr:::poly_clip(pieces[[i]], e[[idx[j]]], "intersection")
            }
          }
        }
        for (ii in which(!id[i, ])) {
          pieces[[i]] <- eulerr:::poly_clip(pieces[[i]], e[[ii]], "minus")
        }
      }

      for (i in seq_along(pieces)) {
        if (is.null(pieces[[i]]$x)) {
          x0 <- lapply(pieces[[i]], "[[", "x")
          y0 <- lapply(pieces[[i]], "[[", "y")
        } else {
          x0 <- pieces[[i]]$x
          y0 <- pieces[[i]]$y
        }
        if (length(x0) > 0L) {
          fills[[i]]$x <- c(x0, recursive = TRUE)
          fills[[i]]$y <- c(y0, recursive = TRUE)
          fills[[i]]$id.lengths <- lengths(x0)
        }
      }
    }
  }

  if (do_labels || do_quantities) {
    singles <- rowSums(id) == 1
    empty <- abs(fitted) < sqrt(.Machine$double.eps)

    centers <- cbind(t(eulerr:::locate_centers(h, k, a, b, phi, fitted)), orig)
    if (do_labels) {
      labels <- list()
      labels$labels <- seq_len(n_e)
      center_labels <- labels$labels[!is.nan(centers[singles, 1L])]
    }

    labels_centers <- centers[!is.nan(centers[, 1L]) & singles, , drop = FALSE]

    droprows <- rep.int(TRUE, NROW(centers))
    for (i in which(is.nan(centers[singles, 1L]))) {
      pick <- id[, i] & !empty & !is.nan(centers[, 1L])
      labels_centers <- rbind(labels_centers, centers[which(pick)[1L], ])
      if (do_labels)
        center_labels <- c(center_labels, labels$labels[i])
      droprows[which(pick)[1]] <- FALSE
    }

    if (do_quantities) {
      quantities <- list()
      quantities$labels <- integer(n_id)
      quantities_centers <-
        centers[!is.nan(centers[, 1L]) & !singles & droprows, , drop = FALSE]
      quantities_centers <- rbind(quantities_centers, labels_centers)
      quantities$labels <- quantities_centers[, 3L]
      quantities$x <- quantities_centers[, 1L]
      quantities$y <- quantities_centers[, 2L]
    }

    if (do_labels) {
      labels$x <- labels_centers[, 1L]
      labels$y <- labels_centers[, 2L]
      labels$labels <- center_labels
    }
  }

  list(ellipses = dd,
       fills = fills,
       edges = edges,
       labels = labels,
       quantities = quantities,
       xlim = limits$xlim,
       ylim = limits$ylim)
}


#' Print (plot) Euler diagram
#'
#' This function is responsible for the actual plotting of
#' `'euler_diagram'` objects created through [plot.euler()] and is usually
#' called automatically after an `'euler'` object is printed on the screen.
#'
#' @param x an object of class `'euler_diagram'`, usually the output of
#'   [plot.euler()].
#' @param ... ignored
#'
#' @return A plot is drawn on the current device using [grid::Grid()].
#' @seealso [plot.euler()], [euler()]
#' @export
#'
#' @examples
#' p <- plot(euler(c(A = 1, B = 3, "A&B" = 0.5)))
#' print(p)
print.euler_diagram <- function(x, ...) {

  euler_grob <- grobify(x)

  groups <- attr(x, "groups")
  do_groups <- !is.null(groups)

  if (do_groups) {
    legend <- x[[1L]]$legend
    pos <- sapply(groups, as.numeric)
    layout <- lengths(lapply(groups, unique))
    if (length(layout) == 1L)
      layout <- c(1L, layout)
    xlim <- range(unlist(lapply(x, "[[", c("limits", "xlim"))))
    ylim <- range(unlist(lapply(x, "[[", c("limits", "ylim"))))
  } else {
    pos <- cbind(1L, 1L)
    legend <- x$legend
    euler_grob <- grid::gList(euler_grob)
    xlim <- x$limits$xlim
    ylim <- x$limits$ylim
    layout <- c(1L, 1L)
  }

  strips <- attr(x, "strips")

  xlim <- grDevices::extendrange(xlim, f = 0.01)
  ylim <- grDevices::extendrange(ylim, f = 0.01)
  xrng <- abs(xlim[1L] - xlim[2L])
  yrng <- abs(ylim[1L] - ylim[2L])
  ar <- xrng/yrng
  adjust <- layout[1L]/layout[2]

  do_strip_left <- layout[1L] > 1L
  do_strip_top <- layout[2L] > 1L
  do_strips <- do_strip_left || do_strip_top
  do_legend <- is.list(legend)

  strip_top_row <- strip_top_col <- strip_left_row <- strip_left_col <- 1

  nrow <- ncol <- 1
  heights <- grid::unit(1, "null")
  widths <- grid::unit(1*ar*layout[2]/layout[1], "null")
  diagram_col <- 1L
  diagram_row <- 1L

  if (do_strip_left) {
    widths <- grid::unit.c(grid::unit(1.5, "lines"), widths)
    diagram_col <- diagram_col + 1L
    ncol <- ncol + 1L
  }
  if (do_strip_top) {
    heights <- grid::unit.c(grid::unit(1.5, "lines"), heights)
    diagram_row <- diagram_row + 1L
    nrow <- nrow + 1L
  }

  if (do_strip_left && do_strip_top) {
    strip_top_col <- strip_top_col + 1L
    strip_left_row <- strip_left_row + 1L
  }

  if (do_legend) {
    legend_side <- legend$side
    do_legend <- TRUE
    legend <- legend[names(legend) != "side"]
    legend_grob <- do.call(grid::legendGrob, legend)
    if (do_strip_top)
      legend_row <- 2

    if (legend_side == "right") {
      # legend on right (default)
      ncol <- ncol + 2L
      legend_row <- nrow
      legend_col <- ncol
      widths <- grid::unit.c(widths, grid::unit(c(1, 1),
                                                c("lines", "grobwidth"),
                                                list(NULL, legend_grob)))
    } else if (legend_side == "left") {
      # legend on left
      ncol <- ncol + 2L
      legend_row <- if (do_strip_top) 2L else 1L
      legend_col <- 1
      diagram_col <- diagram_col + 2L
      if (do_strip_left)
        strip_left_col <- strip_left_col + 2L
      if (do_strip_top)
        strip_top_col <- strip_top_col + 2L
      widths <- grid::unit.c(grid::unit(c(1, 1),
                                        c("grobwidth", "lines"),
                                        list(legend_grob, NULL)), widths)
    } else if (legend_side == "top") {
      # legend on top
      nrow <- nrow + 2L
      legend_row <- 1
      legend_col <- if (do_strip_left) 2L else 1L
      diagram_row <- diagram_row + 2L
      if (do_strip_top)
        strip_top_row <- strip_top_row + 2L
      if (do_strip_left)
        strip_left_row <- strip_left_row + 2L
      heights <- grid::unit.c(grid::unit(c(1, 1),
                                         c("grobheight", "lines"),
                                         list(legend_grob, NULL)),
                              heights)
    } else {
      # legend on bottom
      nrow <- nrow + 2L
      legend_row <- nrow
      legend_col <- if (do_strip_left) 2L else 1L
      heights <- grid::unit.c(heights,
                              grid::unit(c(1, 1),
                                         c("lines", "grobheight"),
                                         list(NULL, legend_grob)))
    }
  } else {
    do_legend <- FALSE
  }

  parent <- grid::viewport(layout = grid::grid.layout(nrow = nrow,
                                                      ncol = ncol,
                                                      widths = widths,
                                                      heights = heights,
                                                      respect = TRUE),
                           name = "canvas")

  panel_layout <- grid::grid.layout(nrow = layout[1L],
                                    ncol = layout[2L],
                                    widths = rep(1/ar, layout[1L]),
                                    heights = rep(1, layout[2L]))

  children <- grid::vpList(grid::viewport(layout.pos.row = diagram_row,
                                          layout.pos.col = diagram_col,
                                          name = "diagram",
                                          layout = panel_layout))
  k <- 2L

  if (do_legend) {
    children[[k]] <- grid::viewport(layout.pos.row = legend_row,
                                    layout.pos.col = legend_col,
                                    name = "legend")
    k <- k + 1L
  }

  if (do_strip_top) {
    strip_top_layout <- grid::grid.layout(nrow = 1L, ncol = layout[2L])
    children[[k]] <- grid::viewport(layout.pos.row = strip_top_row,
                                    layout.pos.col = strip_top_col,
                                    name = "strip.top",
                                    layout = strip_top_layout)
    k <- k + 1L
  }

  if (do_strip_left) {
    strip_left_layout <- grid::grid.layout(nrow = layout[1L], ncol = 1L)
    children[[k]] <- grid::viewport(layout.pos.row = strip_left_row,
                                    layout.pos.col = strip_left_col,
                                    name = "strip.left",
                                    layout = strip_left_layout)
    k <- k + 1L
  }

  grid::grid.newpage()
  grid::pushViewport(grid::vpTree(parent, children))
  grid::upViewport()

  # draw diagram
  grid::downViewport("diagram")
  for (i in seq_along(euler_grob)) {
    if (ncol(pos) == 2L) {
      j <- pos[i, 1L]
      k <- pos[i, 2L]
    } else {
      j <- 1L
      k <- pos[i]
    }
    grid::pushViewport(grid::viewport(layout.pos.row = j,
                                      layout.pos.col = k,
                                      xscale = xlim,
                                      yscale = ylim))
    grid::grid.draw(euler_grob[[i]])
    grid::popViewport()
  }
  grid::upViewport()

  # draw legend
  if (do_legend) {
    grid::downViewport("legend")
    grid::grid.draw(legend_grob)
    grid::upViewport()
  }

  # draw strips
  if (do_strip_top) {
    grid::downViewport("strip.top")
    for (i in seq_len(layout[2L])) {
      grid::pushViewport(grid::viewport(layout.pos.row = 1L,
                                        layout.pos.col = i))
      grid::grid.text(levels(strips$groups[[1L]])[i],
                      just = "bottom",
                      gp = do.call(grid::gpar, strips$gp[i]))
      grid::popViewport()
    }
    grid::upViewport()
  }

  if (do_strip_left) {
    grid::downViewport("strip.left")
    for (i in seq_len(layout[1L])) {
      grid::pushViewport(grid::viewport(layout.pos.row = i,
                                        layout.pos.col = 1))
      grid::grid.text(levels(strips$groups[[2L]])[i],
                      just = "bottom",
                      rot = 90,
                      gp = do.call(grid::gpar, strips$gp[i + layout[1L]]))
      grid::popViewport()
    }
    grid::upViewport()
  }

  grid::popViewport(0)
}
