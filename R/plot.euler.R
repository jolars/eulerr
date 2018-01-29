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

#' Plot area-proportional Euler diagrams
#'
#' Plot Euler diagrams fit with [euler()] using [grid::Grid()] graphics. This
#' function sets up all the necessary plot parameters and computes
#' the geometry of the diagram. [print.euler()], meanwhile,
#' does the actual plotting of the diagram. Please see the **Details** section
#' to learn about the individual settings for each argument.
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
#' The various [grid::gpar()] values that are available for each argument
#' are:
#'
#' \tabular{lcccccc}{
#'              \tab fills \tab edges \tab labels \tab quantities \tab strips \tab legend \cr
#'   col        \tab       \tab x     \tab x      \tab x          \tab x      \tab x      \cr
#'   fill       \tab x     \tab       \tab        \tab            \tab        \tab        \cr
#'   alpha      \tab x     \tab x     \tab x      \tab x          \tab x      \tab x      \cr
#'   lty        \tab       \tab x     \tab        \tab            \tab        \tab        \cr
#'   lwd        \tab       \tab x     \tab        \tab            \tab        \tab        \cr
#'   lex        \tab       \tab x     \tab        \tab            \tab        \tab        \cr
#'   fontsize   \tab       \tab       \tab x      \tab x          \tab x      \tab x      \cr
#'   cex        \tab       \tab       \tab x      \tab x          \tab x      \tab x      \cr
#'   fontfamily \tab       \tab       \tab x      \tab x          \tab x      \tab x      \cr
#'   lineheight \tab       \tab       \tab x      \tab x          \tab x      \tab x      \cr
#'   font       \tab       \tab       \tab x      \tab x          \tab x      \tab x
#' }
#'
#' Defaults for these values, as well as other parameters of the plots, can
#' be set globally using [eulerr_options()].
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
#' @param legend a logical scalar or list. If a list,
#'   the item `side` can be used to set the location of the legend. See
#'   [grid::grid.legend()].
#' @param labels a logical, vector, or list. Vectors are assumed to be
#'   text for the labels. See [grid::grid.text()].
#' @param quantities a logical, vector, or list. Vectors are assumed to be
#'   text for the quantities' labels, which by
#'   default are the original values in the input to [euler()]. See
#'   [grid::grid.text()].
#' @param edges a logical, vector, or list. Vectors are assumed to be
#'   colors for edges of the ellipses. See [grid::grid.polyline()].
#' @param fills a logical, vector, or list. Vectors are assumed to be
#'   colors to fill the shapes in the diagram. See [grid::grid.path()].
#' @param n number of vertices for the ellipses
#' @param strips a list. Will be ignored unless the `'by'` argument
#'   was used in [euler()].
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
#' @seealso [euler()], [print.euler()], [grid::gpar()],
#'   [grid::grid.polygon()], [grid::grid.polyline()], [grid::grid.path()],
#'   [grid::grid.legend()], [grid::grid.text()]
#'
#' @return Provides an object of class `'euler', 'diagram'` , which is a
#'   description of the diagram to be drawn. [print.euler()] does the actual
#'   plotting of the diagram and is usually called automatically after this
#'   function is called.
#' @export
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
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
                       legend = FALSE,
                       fills = TRUE,
                       edges = TRUE,
                       labels = identical(legend, FALSE),
                       quantities = FALSE,
                       strips = NULL,
                       n = 200,
                       ...,
                       fill,
                       fill_alpha,
                       auto.key,
                       fontface,
                       par.settings,
                       default.prepanel,
                       default.scales,
                       panel) {

  stopifnot(n > 0, is.numeric(n) && length(n) == 1)

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

  opar <- eulerr_options()

  groups <- attr(x, "groups")
  do_groups <- !is.null(groups)
  do_labels <-
    is.list(labels) ||
    isTRUE(labels) || is.character(labels) || is.expression(labels)
  do_quantities <-
    is.list(quantities) ||
    isTRUE(quantities) ||
    is.character(quantities) || is.expression(quantities)
  do_edges <-
    is.list(edges) ||
    isTRUE(edges) || is.numeric(edges) || is.character(edges)
  do_fills <-
    is.list(fills) ||
    isTRUE(fills) || is.numeric(fills) || is.character(fills)
  do_legend <- is.list(legend) || isTRUE(legend)
  do_strips <- !is.null(groups)

  ellipses <- if (do_strips) x[[1L]]$coefficients else x$coefficients

  n_e <- NROW(ellipses)
  n_id <- 2^n_e - 1
  id <- bit_indexr(n_e)

  setnames <- rownames(ellipses)

  # setup fills
  if (do_fills) {
    if (isTRUE(fills)) {
      fills <- list(fill = eulerr_pal(n_e))
    } else if (is.list(fills)) {
      if (is.function(fills$fill))
        fills$fill <- fills$fill(n_e)
      else if (is.null(fills$fill))
        fills$fill <- opar$fills$fill(n_e)
      else
        fills$fill <- fills$fill
    } else {
      if (is.function(fills))
        fills <- list(fill = fills(n_e))
      else
        fills <- list(fill = fills)
    }

    n_fills <- length(fills$fill)
    if (n_fills < n_id) {
      for (i in (n_fills + 1L):n_id) {
        fills$fill[i] <- mix_colors(fills$fill[which(id[i, ])])
      }
    }

    fills$gp <- setup_gpar(list(fill = fills$fill, alpha = opar$fills$alpha),
                           fills, n_id)
  } else {
    fills <- NULL
  }

  # setup edges
  if (do_edges) {
    if (isTRUE(edges))
      edges <- list()
    else if (!is.list(edges))
      edges <- list(col = edges)

    edges$gp <- setup_gpar(opar$edges, edges, n_e)
  } else {
    edges <- NULL
  }

  # setup strips
  if (do_groups) {
    group_names <- lapply(groups, levels)
    n_levels <- sum(lengths(group_names))

    if (isTRUE(strips)) {
      strips <- list()
    }

    strips$groups <- groups
    strips$gp <- setup_gpar(opar$strips, strips, n_levels)
  } else {
    strips <- NULL
  }

  # setup labels
  if (do_labels) {
    if (is.list(labels)) {
      if (is.null(labels$labels))
        labels$labels <- setnames
    } else if (isTRUE(labels)) {
      labels <- list(labels = setnames)
    } else {
      labels <- list(labels = labels)
    }

    labels$gp <- setup_gpar(opar$labels, labels, n_e)
  } else {
    labels <- NULL
  }

  # setup quantities
  if (do_quantities) {
    if (!is.list(quantities)) {
      quantities <- list(labels = NULL)
    }
    quantities$gp <- setup_gpar(opar$quantities, quantities, n_id)
  } else {
    quantities <- NULL
  }

  # setup legend
  if (do_legend) {
    if (is.list(legend)) {
      if (is.null(legend$labels)) {
        if (is.list(labels)) {
          if (!is.null(labels$labels)) {
            legend$labels <- labels$labels
          }
        } else {
          legend$labels <- setnames
        }
      } else {
        legend$labels <- legend$labels
      }
    } else {
      if (do_groups) {
        legend <- list(labels = rownames(x[[1L]]$coefficients))
      } else {
        legend <- list(labels = rownames(x$coefficients))
      }
    }
    opar$legend$ncol <- 1L
    opar$legend$nrow <- n_e

    legend <- replace_list(opar$legend, legend)

    legend$gp <- setup_gpar(
      list(
        fill = if (do_fills)
          mapply(grDevices::adjustcolor,
                 col = fills$gp$fill,
                 alpha.f = fills$gp$alpha)
        else "transparent",
        cex = legend$cex,
        fontsize = legend$fontsize/legend$cex,
        col = if (do_edges)
          mapply(grDevices::adjustcolor,
                 col = edges$gp$col,
                 alpha.f = edges$gp$alpha)
        else "transparent"),
      legend,
      n_e
    )
  } else {
    legend <- NULL
  }

  if (do_groups) {
    data <- lapply(x,
                   setup_geometry,
                   fills = fills,
                   edges = edges,
                   labels = labels,
                   quantities = quantities,
                   n = n,
                   id = id)
  } else {
    data <- setup_geometry(x,
                           fills,
                           edges,
                           labels,
                           quantities,
                           n,
                           id)
  }

  structure(list(fills = fills,
                 edges = edges,
                 labels = labels,
                 quantities = quantities,
                 strips = strips,
                 legend = legend,
                 data = data),
            class = c("euler", "diagram"))
}

#' Compute geometries and label locations
#'
#' @param x an object of class 'euler'
#' @param do_fills do fills?
#' @param do_edges do edges?
#' @param do_labels do labels?
#' @param do_quantities do quantities?
#'
#' @return a list object with slots for the various objects
#' @keywords internal
setup_geometry <- function(x,
                           fills,
                           edges,
                           labels,
                           quantities,
                           n,
                           id) {
  dd <- x$coefficients
  orig <- x$original.values
  fitted <- x$fitted.values

  do_fills <- !is.null(fills)
  do_edges <- !is.null(edges)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- NROW(dd)
  n_id <- 2L^n_e - 1L

  e <- ellipse(h, k, a, b, phi, n)
  e_x <- c(lapply(e, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(e, "[[", "y"), recursive = TRUE)

  limits <- get_bounding_box(h, k, a, b, phi)

  # setup edges
  if (do_edges)
    edges <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))

  if (do_fills) {
    # overlay ellipses on top of each other
    pieces <- fills <- vector("list", n_id)
    for (i in rev(seq_len(n_id))) {
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
      for (ii in which(!id[i, ])) {
        pieces[[i]] <- poly_clip(pieces[[i]], e[[ii]], "minus")
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

    centers <- cbind(t(locate_centers(h, k, a, b, phi, fitted)), seq_len(n_id))
    rownames(centers) <- names(orig)

    if (do_labels) {
      labels <- list(labels = labels$labels)
      center_labels <- labels$labels[!is.nan(centers[singles, 1L])]
    }

    labels_centers <- centers[!is.nan(centers[, 1L]) & singles, , drop = FALSE]

    droprows <- rep.int(TRUE, NROW(centers))
    for (i in which(is.nan(centers[singles, 1L]))) {
      pick <- id[, i] & !empty & !is.nan(centers[, 1L])
      labels_centers <- rbind(labels_centers, centers[which(pick)[1L], ])
      if (do_labels)
        center_labels <- c(center_labels, labels$labels[i])
      droprows[which(pick)[1L]] <- FALSE
    }

    if (do_quantities) {
      quantities_centers <-
        centers[!is.nan(centers[, 1L]) & !singles & droprows, , drop = FALSE]
      quantities_centers <- rbind(quantities_centers, labels_centers)
      if (!is.null(quantities$labels))
        quantities <- list(labels = quantities$labels[quantities_centers[, 3L]])
      else
        quantities <- list(labels = orig[quantities_centers[, 3L]])
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
       fitted.values = fitted,
       original.values = orig,
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
#' `'euler_diagram'` objects created through [plot.euler()].
#'
#' @param x an object of class `'euler'`, usually the output of
#'   [plot.euler()].
#' @return A plot is drawn on the current device using [grid::Grid()].
#' @keywords internal
print_diagram <- function(x) {
  legend <- x$legend
  edges <- x$edges
  labels <- x$labels
  quantities <- x$quantities
  data <- x$data
  strips <- x$strips
  groups <- strips$groups

  opar <- eulerr_options()

  do_legend <- !is.null(legend)
  do_groups <- !is.null(strips)

  if (do_groups) {
    euler_grob <- lapply(
      data,
      setup_grobs,
      gp_fills = x$fills$gp,
      gp_edges = x$edges$gp,
      gp_labels = x$labels$gp,
      gp_quantities = x$quantities$gp
    )
    euler_grob <- do.call(grid::gList, euler_grob)
    pos <- vapply(groups, as.numeric, numeric(NROW(groups)), USE.NAMES = FALSE)
    layout <- lengths(lapply(groups, unique))
    if (length(layout) == 1L)
      layout <- c(1L, layout)
    xlim <- range(unlist(lapply(data, "[[", "xlim")))
    ylim <- range(unlist(lapply(data, "[[", "ylim")))
  } else {
    euler_grob <- setup_grobs(data,
                              gp_fills = x$fills$gp,
                              gp_edges = x$edges$gp,
                              gp_labels = x$labels$gp,
                              gp_quantities = x$quantities$gp)
    euler_grob <- grid::gList(euler_grob)
    xlim <- data$xlim
    ylim <- data$ylim
    pos <- c(1L, 1L)
    layout <- c(1L, 1L)
  }

  xlim <- grDevices::extendrange(xlim, f = 0.01)
  ylim <- grDevices::extendrange(ylim, f = 0.01)
  xrng <- abs(xlim[1L] - xlim[2L])
  yrng <- abs(ylim[1L] - ylim[2L])
  ar <- xrng/yrng
  adjust <- layout[1L]/layout[2]

  do_strip_left <- layout[1L] > 1L
  do_strip_top <- layout[2L] > 1L
  do_strips <- do_strip_left || do_strip_top

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
    legend_grob <- grid::legendGrob(
      labels = legend$labels,
      do.lines = legend$do.lines,
      ncol = legend$ncol,
      nrow = legend$nrow,
      hgap = legend$hgap,
      vgap = legend$vgap,
      default.units = legend$default.units,
      pch = legend$pch,
      gp = legend$gp
    )
    if (do_strip_top)
      legend_row <- 2

    if (legend$side == "right") {
      # legend on right (default)
      ncol <- ncol + 2L
      legend_row <- nrow
      legend_col <- ncol
      widths <- grid::unit.c(widths, grid::unit(c(1, 1),
                                                c("lines", "grobwidth"),
                                                list(NULL, legend_grob)))
    } else if (legend$side == "left") {
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
    } else if (legend$side == "top") {
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
                           name = "canvas",
                           gp = grid::gpar(fontsize = opar$fontsize))

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
    if (NCOL(pos) == 2L) {
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
  invisible(x)
}


#' Grobify Euler objects
#'
#' @param x an `euler` object
#'
#' @return A [grid::gList()] is returned.
#' @keywords internal
setup_grobs <- function(x,
                        gp_labels,
                        gp_edges,
                        gp_fills,
                        gp_quantities) {
  labels <- x$labels
  edges <- x$edges
  fills <- x$fills
  quantities <- x$quantities
  fitted <- x$fitted.values

  do_labels <- !is.null(labels)
  do_edges <- !is.null(edges)
  do_fills <- !is.null(fills)
  do_quantities <- !is.null(quantities)

  n_e <- nrow(x$ellipses)
  n_id <- 2L^n_e - 1L
  id <- bit_indexr(n_e)

  #edges
  if (do_edges) {
    # edges
    edges_grob <- grid::polylineGrob(edges$x,
                                     edges$y,
                                     id.lengths = edges$id.lengths,
                                     default.units = "native",
                                     gp = gp_edges)
  } else {
    edges_grob <- grid::nullGrob()
  }

  # fills
  if (do_fills) {
    fills_grob <- vector("list", n_id)
    fill_id <- seq_len(n_id)
    for (i in rev(seq_len(n_id))) {
      idx <- id[i, ]
      n_idx <- sum(idx)
      sub_id <- colSums(t(id)[idx, , drop = FALSE]) == n_idx
      comeone <- rowSums(id[rowSums(id) > n_idx & sub_id & fitted > sqrt(.Machine$double.eps), , drop = FALSE])
      if (NROW(comeone) > 0) {
        nextup <- min(comeone)
        j <- which(rowSums(id) == nextup & sub_id & fitted > sqrt(.Machine$double.eps))
      } else {
        j <- i
      }
      if (is.null(fills[[i]])) {
        fill_id[j] <- fill_id[i]
      }
    }

    for (i in seq_len(n_id)) {
      if (is.null(fills[[i]])) {
        fills_grob[[i]] <- grid::nullGrob()
      } else
        fills_grob[[i]] <- grid::pathGrob(
          fills[[i]]$x,
          fills[[i]]$y,
          id.lengths = fills[[i]]$id.lengths,
          default.units = "native",
          gp = gp_fills[fill_id[i]]
          #gp = gp_fills[i]
        )
    }
    fills_grob <- do.call(grid::gList, fills_grob)
  } else {
    fills_grob <- grid::nullGrob()
  }

  if (do_quantities)  {
    quantities_id <- rep.int(FALSE, length(quantities$x))
    if (do_labels)
      quantities_id[names(quantities$x) %in% names(labels$x)] <- TRUE
    lab_id <- quantities_id

    quantities_grob <- grid::textGrob(
      label = quantities$labels,
      x = quantities$x,
      y = quantities$y,
      vjust = ifelse(lab_id & do_labels, 1, 0.5),
      name = "quantities",
      default.units = "native",
      gp = gp_quantities
    )
  } else {
    quantities_grob <- grid::nullGrob()
  }

  # labels
  if (do_labels) {
    labels_grob <- grid::textGrob(
      label = labels$labels,
      x = labels$x,
      y = labels$y,
      vjust = if (do_quantities) -0.5 else 0.5,
      name = "labels",
      default.units = "native",
      gp = gp_labels
    )
  } else {
    labels_grob <- grid::nullGrob()
  }
  out <- grid::grobTree(fills_grob, edges_grob, labels_grob, quantities_grob)
}
