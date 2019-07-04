#' Plot Euler and Venn diagrams
#'
#' Plot diagrams fit with [euler()] and [venn()] using [grid::Grid()] graphics.
#' This
#' function sets up all the necessary plot parameters and computes
#' the geometry of the diagram. [plot.eulergram()], meanwhile,
#' does the actual plotting of the diagram. Please see the **Details** section
#' to learn about the individual settings for each argument.
#'
#' The only difference between [plot.euler()] and [plot.venn()] is that
#' `quantities` is set to `TRUE` by default in the latter and `FALSE` in
#' the former.
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
#' \tabular{lccccccccc}{
#'              \tab fills \tab edges \tab labels \tab quantities \tab percentages \tab strips \tab legend \tab main \cr
#'   col        \tab       \tab x     \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   fill       \tab x     \tab       \tab        \tab            \tab             \tab        \tab        \tab      \cr
#'   alpha      \tab x     \tab x     \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   lty        \tab       \tab x     \tab        \tab            \tab             \tab        \tab        \tab      \cr
#'   lwd        \tab       \tab x     \tab        \tab            \tab             \tab        \tab        \tab      \cr
#'   lex        \tab       \tab x     \tab        \tab            \tab             \tab        \tab        \tab      \cr
#'   fontsize   \tab       \tab       \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   cex        \tab       \tab       \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   fontfamily \tab       \tab       \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   lineheight \tab       \tab       \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
#'   font       \tab       \tab       \tab x      \tab x          \tab x           \tab x      \tab x      \tab x    \cr
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
#' @param fills a logical, vector, or list of graphical parameters for the fills
#'   in the diagram. Vectors are assumed to be colors for the fills.
#'   See [grid::grid.path()].
#' @param edges a logical, vector, or list of graphical parameters for the edges
#'   in the diagram. Vectors are assumed to be colors for the edges.
#'   See [grid::grid.polyline()].
#' @param legend a logical scalar or list. If a list,
#'   the item `side` can be used to set the location of the legend. See
#'   [grid::grid.legend()].
#' @param labels a logical, vector, or list. Vectors are assumed to be
#'   text for the labels. See [grid::grid.text()].
#' @param quantities a logical, vector, or list. Vectors are assumed to be
#'   text for the quantities' labels, which by
#'   default are the original values in the input to [euler()]. See
#'   [grid::grid.text()].
#' @param strips a list, ignored unless the `'by'` argument
#'   was used in [euler()]
#' @param n number of vertices for the `edges` and `fills`
#' @param main a title for the plot in the form of a
#'   character, expression, list or something that can be
#'   sensibly converted to a label via [grDevices::as.graphicsAnnot()]. A
#'   list of length one can be provided, in which case its only element
#'   is used as the label. If a list of longer length is provided, an item
#'   named `'label'` must be provided (and will be used for the actual text).
#' @param ... parameters to update `fills` and `edges` with and thereby a shortcut
#'   to set these parameters
#' @param percentages a logical, vector, or list. If `TRUE`, the
#'   percentages of the original set overlaps relative masses will be plotted
#'   underneath the label and quantity for each overlap's tag.
#'   Vectors are assumed to be
#'   text for the percentages' labels, which by
#'   default are the original values in the input to [euler()]. See
#'   [grid::grid.text()].
#'
#' @seealso [euler()], [plot.eulergram()], [grid::gpar()],
#'   [grid::grid.polyline()], [grid::grid.path()],
#'   [grid::grid.legend()], [grid::grid.text()]
#'
#' @return Provides an object of class `'eulergram'` , which is a
#'   description of the diagram to be drawn. [plot.eulergram()] does the actual
#'   drawing of the diagram.
#' @export
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
#'
#' # Customize colors, remove borders, bump alpha, color labels white
#' plot(fit,
#'      fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
#'      labels = list(col = "white", font = 4))
#'
#' # Add quantities to the plot
#' plot(fit, quantities = TRUE)
#'
#' # Add a custom legend and retain quantities
#' plot(fit, quantities = TRUE, legend = list(labels = c("foo", "bar")))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit, fill = "transparent", lty = 1:2)
#'
#' # Save plot parameters to plot using some other method
#' diagram_description <- plot(fit)
#'
#' # Plots using 'by' argument
#' plot(euler(fruits[, 1:4], by = list(sex)), legend = TRUE)
plot.euler <- function(x,
                       fills = TRUE,
                       edges = TRUE,
                       legend = FALSE,
                       labels = identical(legend, FALSE),
                       quantities = FALSE,
                       percentages = FALSE,
                       strips = NULL,
                       main = NULL,
                       n = 200L,
                       ...) {

  # retrieve default options
  opar <- eulerr_options()

  groups <- attr(x, "groups")
  dots <- list(...)

  do_custom_legend <- grid::is.grob(legend)

  do_fills <- !is_false(fills) && !is.null(fills)
  do_edges <- !is_false(edges) && !is.null(edges)
  do_labels <- !is_false(labels) && !is.null(labels)
  do_quantities <- !is_false(quantities) && !is.null(quantities)
  do_percentages <- !is_false(percentages) && !is.null(percentages)
  do_legend <- !is_false(legend) && !is.null(legend)
  do_groups <- !is.null(groups)
  do_strips <- !is_false(strips) && do_groups
  do_main <- is.character(main) || is.expression(main) || is.list(main)

  ellipses <- if (do_groups) x[[1L]]$ellipses else x$ellipses

  n_e <- NROW(ellipses)
  n_id <- 2^n_e - 1
  id <- bit_indexr(n_e)

  setnames <- rownames(ellipses)

  stopifnot(n > 0, is.numeric(n) && length(n) == 1)

  # setup fills
  if (do_fills) {
    fills_out <- replace_list(
      list(fill = opar$fills$fill,
           alpha = opar$fills$alpha),
      if (is.list(fills))
        fills
      else if (isTRUE(fills))
        list()
      else
        list(fill = fills)
    )
    fills_out <- replace_list(fills_out, dots)
    fills_out$col <- "transparent"

    if (is.function(fills_out$fill))
      fills_out$fill <- fills_out$fill(n_e)

    n_fills <- length(fills_out$fill)
    if (n_fills < n_id) {
      for (i in (n_fills + 1L):n_id) {
        fills_out$fill[i] <- mix_colors(fills_out$fill[which(id[i, ])])
      }
    }
    fills <- list()
    fills$gp <- setup_gpar(fills_out, list(), n_id)
  } else {
    fills <- NULL
  }

  # setup edges
  if (do_edges) {
    if (isTRUE(edges))
      edges_out <- list()
    else if (!is.list(edges))
      edges_out <- list(col = edges)
    else
      edges_out <- edges

    edges <- list()
    edges$gp <- setup_gpar(list(col = opar$edges$col,
                                alpha = opar$edges$alpha,
                                lex = opar$edges$lex,
                                lwd = opar$edges$lwd,
                                lty = opar$edges$lty),
                           update_list(edges_out, dots),
                           n_e)
  } else {
    edges <- NULL
  }

  # setup strips
  if (do_groups) {
    group_names <- lapply(groups, levels)
    n_levels <- sum(lengths(group_names))
  }

  if (do_strips) {
    strips <- list(gp = setup_gpar(opar$strips, strips, n_levels),
                   groups = groups)
  } else {
    strips <- NULL
  }

  # setup labels
  if (do_labels) {
    if (is.list(labels)) {
      labels <- update_list(list(labels = setnames,
                                 rot = opar$labels$rot),
                            labels)
    } else if (isTRUE(labels)) {
      labels <- list(labels = setnames,
                     rot = opar$labels$rot)
    } else {
      labels <- list(labels = labels,
                     rot = opar$labels$rot)
    }

    labels$rot <- rep_len(labels$rot, n_e)
    labels$gp <- setup_gpar(list(col = opar$labels$col,
                                 alpha = opar$labels$alpha,
                                 fontsize = opar$labels$fontsize,
                                 cex = opar$labels$cex,
                                 fontfamily = opar$labels$fontfamily,
                                 lineheight = opar$labels$lineheight,
                                 font = opar$labels$font),
                            labels,
                            n_e)
  } else {
    labels <- NULL
  }

  # setup quantities
  if (do_quantities) {
    if (is.list(quantities)) {
      quantities <- update_list(list(labels = NULL,
                                     rot = opar$quantities$rot),
                                quantities)
    } else if (isTRUE(quantities)) {
      quantities <- list(labels = NULL, rot = opar$quantities$rot)
    } else {
      quantities <- list(labels = quantities, rot = opar$quantities$rot)
    }
    quantities$rot <- rep_len(quantities$rot, n_id)

    quantities$gp <- setup_gpar(list(col = opar$quantities$col,
                                     alpha = opar$quantities$alpha,
                                     fontsize = opar$quantities$fontsize,
                                     cex = opar$quantities$cex,
                                     fontfamily = opar$quantities$fontfamily,
                                     lineheight = opar$quantities$lineheight,
                                     font = opar$quantities$font),
                                quantities,
                                n_id)
  } else {
    quantities <- NULL
  }

  # setup percentages
  if (do_percentages) {
    if (is.list(percentages)) {
      percentages <- update_list(list(labels = NULL,
                                      rot = opar$percentages$rot),
                                 percentages)
    } else if (isTRUE(percentages)) {
      percentages <- list(labels = NULL, rot = opar$percentages$rot)
    } else {
      percentages <- list(labels = percentages, rot = opar$percentages$rot)
    }
    percentages$rot <- rep_len(percentages$rot, n_id)

    percentages$gp <- setup_gpar(list(col = opar$percentages$col,
                                     alpha = opar$percentages$alpha,
                                     fontsize = opar$percentages$fontsize,
                                     cex = opar$percentages$cex,
                                     fontfamily = opar$percentages$fontfamily,
                                     lineheight = opar$percentages$lineheight,
                                     font = opar$percentages$font),
                                percentages,
                                n_id)
  } else {
    percentages <- NULL
  }

  # setup legend
  if (do_custom_legend) {
    legend <- legend
  } else if (do_legend) {
    # TODO: create a better, custom legend

    legend <- update_list(
      list(labels = if (do_labels) labels$labels else setnames,
           side = opar$legend$side,
           nrow = n_e,
           ncol = 1L,
           byrow = opar$legend$byrow,
           do.lines = opar$legend$do.lines,
           lines.first = opar$legend$lines.first,
           hgap = opar$legend$hgap,
           vgap = opar$legend$vgap,
           default.units = opar$legend$default.units,
           pch = opar$legend$pch),
      legend
    )

    legend$gp <- setup_gpar(
      list(
        fill = if (do_fills) fills$gp$fill else "transparent",
        alpha = if (do_fills)
          fills$gp$alpha
        else if (do_edges)
          edges$gp$alpha
        else
          0,
        cex = opar$legend$cex,
        fontsize = opar$legend$fontsize/opar$legend$cex,
        font = opar$legend$font,
        fontfamily = opar$legend$fontfamily,
        lwd = if (do_edges) edges$gp$lwd else 0,
        lex = if (do_edges) edges$gp$lex else 0,
        col = if (do_edges) edges$gp$col else "transparent"),
      legend,
      n_e
    )
  } else {
    legend <- NULL
  }

  if (do_main) {
    if (is.list(main)) {
      if (length(main) == 1 && is.null(names(main)))
        label <- main[[1]]
      else
        label <- main$label

      if (is.null(label))
        stop("you need to provide a 'label' item if 'main' is a list.")

    } else {
      label <- main
    }

    main <- update_list(
      list(label = label,
           x = opar$main$x,
           y = opar$main$y,
           just = opar$main$just,
           hjust = opar$main$hjust,
           vjust = opar$main$vjust,
           rot = opar$main$rot,
           check.overlap = opar$main$check.overlap,
           default.units = opar$main$default.units),
      main
    )

    main$gp <- setup_gpar(
      list(
        cex = opar$main$cex,
        fontsize = opar$main$fontsize,
        font = opar$main$font,
        fontfamily = opar$main$fontfamily,
        col = opar$main$col,
        lineheight = opar$main$lineheight,
        alpha = opar$main$alpha
      ),
      main,
      1
    )
  } else {
    main <- NULL
  }

  # set up geometry for diagrams
  if (do_groups) {
    data <- lapply(x,
                   setup_geometry,
                   fills = fills,
                   edges = edges,
                   labels = labels,
                   quantities = quantities,
                   percentages = percentages,
                   n = n,
                   id = id)
  } else {
    data <- setup_geometry(x,
                           fills,
                           edges,
                           labels,
                           quantities,
                           percentages,
                           n,
                           id)
  }

  # start setting up grobs

  if (do_groups) {
    n_groups <- length(data)
    euler_grob_children <- grid::gList()
    for (i in seq_len(n_groups)) {
      euler_grob_children[[i]] <- setup_grobs(data[[i]],
                                              fills = fills,
                                              edges = edges,
                                              labels = labels,
                                              quantities = quantities,
                                              percentages = percentages,
                                              number = i)
    }
    euler_grob <- grid::gTree(grid::nullGrob(),
                              name = "canvas.grob",
                              children = euler_grob_children)
    pos <- vapply(groups, as.numeric, numeric(NROW(groups)), USE.NAMES = FALSE)
    layout <- lengths(lapply(groups, unique))
    if (length(layout) == 1L)
      layout <- c(1L, layout)
    xlim <- range(unlist(lapply(data, "[[", "xlim")))
    ylim <- range(unlist(lapply(data, "[[", "ylim")))
  } else {
    euler_grob <- setup_grobs(data,
                              fills = fills,
                              edges = edges,
                              labels = labels,
                              quantities = quantities,
                              percentages = percentages,
                              number = 1)
    euler_grob <- grid::grobTree(euler_grob,
                                 name = "canvas.grob")
    xlim <- data$xlim
    ylim <- data$ylim
    pos <- c(1L, 1L)
    layout <- c(1L, 1L)
  }

  xlim <- grDevices::extendrange(xlim, f = 0.01)
  ylim <- grDevices::extendrange(ylim, f = 0.01)
  xrng <- abs(xlim[1L] - xlim[2L])
  yrng <- abs(ylim[1L] - ylim[2L])

  if (xrng == 0 || yrng == 0) {
    xrng <- yrng <- 1
    ylim <- xlim <- c(0, 1)
  }

  ar <- xrng/yrng
  # adjust <- layout[1L]/layout[2]

  do_strip_left <- layout[1L] > 1L && do_strips
  do_strip_top <- layout[2L] > 1L && do_strips

  strip_top_row <- strip_top_col <- strip_left_row <- strip_left_col <- 1

  nrow <- ncol <- 1
  heights <- grid::unit(1, "null")
  widths <- grid::unit(1*ar*layout[2]/layout[1], "null")
  diagram_col <- 1
  diagram_row <- 1

  if (do_main) {
    diagram_row <- diagram_row + 2
    nrow <- nrow + 2
    strip_left_row <- strip_left_row + 2
    strip_top_row <- strip_top_row + 2
  }

  if (do_strip_left) {
    diagram_col <- diagram_col + 1
    ncol <- ncol + 1
  }
  if (do_strip_top) {
    diagram_row <- diagram_row + 1
    nrow <- nrow + 1
  }

  if (do_strip_left && do_strip_top) {
    strip_top_col <- strip_top_col + 1
    strip_left_row <- strip_left_row + 1
  }

  # draw strips
  if (do_strip_top) {
    strip_top_vp <-
      grid::viewport(layout.pos.row = strip_top_row,
                     layout.pos.col = strip_top_col,
                     name = "strip.top.vp",
                     layout = grid::grid.layout(nrow = 1, ncol = layout[2]))

    lvls <- levels(strips$groups[[1]])
    n_lvls <- length(lvls)
    step <- 1/n_lvls

    strip_top_grob <- grid::textGrob(lvls,
                                     x = step/2 + (seq(0, n_lvls - 1)*step),
                                     name = "strip.top.grob",
                                     gp = do.call(grid::gpar, strips$gp),
                                     vp = strip_top_vp)

    heights <- grid::unit.c(grid::unit(2, "grobheight", list(strip_top_grob)),
                            heights)
  }

  if (do_strip_left) {
    strip_left_vp <-
      grid::viewport(layout.pos.row = strip_left_row,
                     layout.pos.col = strip_left_col,
                     name = "strip.left.vp",
                     layout = grid::grid.layout(nrow = layout[1], ncol = 1))

    lvls <- levels(strips$groups[[2]])
    n_lvls <- length(lvls)
    step <- 1/n_lvls

    strip_left_grob <- grid::textGrob(lvls,
                                      y = step/2 + (seq(0, n_lvls - 1)*step),
                                      name = "strip.left.grob",
                                      rot = 90,
                                      gp = do.call(grid::gpar, strips$gp),
                                      vp = strip_left_vp)

    widths <- grid::unit.c(grid::unit(2, "grobwidth", list(strip_left_grob)),
                           widths)
  }

  if (do_legend) {
    if (do_custom_legend) {
      legend_grob <- legend
      legend <- list(side = "right")
    } else {
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
    }

    legend_grob$name <- "legend.grob"
    if (do_strip_top)
      legend_row <- 2 + 2*do_main

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
      legend_row <- if (do_strip_top) 2L + 2*do_main else 1L + 2*do_main
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
      legend_row <- 1L + do_main*2
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
    legend_grob$vp <- grid::viewport(layout.pos.row = legend_row,
                                     layout.pos.col = legend_col,
                                     name = "legend.vp")
  }

  if (do_main) {
    main_grob <- grid::textGrob(label = main$label,
                                x = main$x,
                                y = main$y,
                                just = main$just,
                                hjust = main$hjust,
                                vjust = main$vjust,
                                rot = main$rot,
                                check.overlap = FALSE,
                                default.units = main$default.units,
                                gp = main$gp,
                                name = "main.grob")
    heights <- grid::unit.c(grid::unit(c(1, 1),
                                       c("lines", "grobheight"),
                                       list(NULL, main_grob)),
                            heights)
    main_grob$vp <- grid::viewport(layout.pos.row = 1,
                                   layout.pos.col = diagram_col,
                                   name = "main.vp")
  }

  canvas_vp <- grid::viewport(
    layout.pos.row = diagram_row,
    layout.pos.col = diagram_col,
    name = "canvas.vp",
    layout = grid::grid.layout(nrow = layout[1L],
                               ncol = layout[2L],
                               widths = rep(1/ar, layout[1L]),
                               heights = rep(1, layout[2L]))
  )

  for (i in seq_along(euler_grob$children)) {
    if (NCOL(pos) == 2L) {
      j <- pos[i, 1L]
      k <- pos[i, 2L]
    } else {
      j <- 1L
      k <- pos[i]
    }
    euler_grob$children[[i]]$vp <- grid::viewport(
      layout.pos.row = j,
      layout.pos.col = k,
      xscale = if (xlim[1] == -Inf) c(-1, 1) else xlim,
      yscale = if (ylim[1] == -Inf) c(-1, 1) else ylim,
      name = paste0("panel.vp.", j, ".", k)
    )
  }

  euler_grob$vp <- canvas_vp

    # return a gTree object
  grid::grobTree(
    if (do_main) main_grob = main_grob,
    if (do_strip_top) strip_top_grob = strip_top_grob,
    if (do_strip_left) strip_left_grob = strip_left_grob,
    if (do_legend) legend_grob = legend_grob,
    euler_grob = euler_grob,
    vp = grid::viewport(layout = grid::grid.layout(nrow = nrow,
                                                   ncol = ncol,
                                                   widths = widths,
                                                   heights = heights,
                                                   respect = TRUE),
                        name = "euler.vp"),
    cl = "eulergram",
    name = "euler.diagram"
  )
}

#' Test if two polygons are intersecting or not
#'
#' @param a first polygon
#' @param b second polygon
#'
#' @return `TRUE` if polygon `a` and `b` are intersecting, `FALSE` otherwise.
#' @keywords internal
#' @noRd
test_intersection <- function(a, b) {
  length(poly_clip(a, b, "intersection")) > 0
}

locate_centers <- function(p, precision = 1) {
  n_p <- length(p)

  if (n_p == 1) {
    polylabelr::poi(p[[1]]$x, p[[1]]$y, precision = precision)
  } else if (n_p > 1) {

    intersects <- matrix(TRUE, ncol = n_p, nrow = n_p)

    for (i in 1:(n_p - 1)) {
      for (j in (i + 1):n_p) {
        intersects[i, j] <- test_intersection(p[[i]], p[[j]])
      }
    }

    intersects[lower.tri(intersects)] <- intersects[upper.tri(intersects)]

    clusters <- unique(lapply(split(intersects, row(intersects)), which))

    res <- lapply(clusters, function(cluster) {
      n_c <- length(cluster)
      x <- y <- double(0)

      for (i in seq_len(n_c)) {
        x <- c(x, p[[cluster[i]]]$x)
        y <- c(y, p[[cluster[i]]]$y)
        if (i < n_c) {
          x <- c(x, NA)
          y <- c(y, NA)
        }
      }
      polylabelr::poi(x, y, precision = precision)
    })

    res[[which.max(unlist(lapply(res, "[[", "dist")))]]

  } else {
    grDevices::xy.coords(NA, NA)
  }
}

#' Setup grobs for labels (labels, quantities, percentages)
#'
#' @param data data for the locations of points and more
#' @param labels plot parameters for labels
#' @param quantities plot parameters for quantities
#' @param percentages plot parameters for percentages
#'
#' @return A [grid::gTree()] object
#' @keywords internal
setup_tag <- function(data, labels, quantities, percentages) {

  x <- data$x
  y <- data$y

  label <- data$labels
  quantity <- data$quantities
  percentage <- data$percentages

  k <- data$par_id

  do_labels <- !is.null(labels) & !is.na(label)
  do_quantities <- !is.null(quantities) & !is.na(quantity)
  do_percentages <- !is.null(percentages) & !is.na(percentage)

  n_grobs <- sum(do_labels + do_quantities + do_percentages)
  n_rows <- n_grobs * 2 - 1

  # setup a gList to store the various components of the tag
  grobs <- gList()
  i <- 0
  j <- 0

  if (do_labels) {
    i <- i + 1
    grobs[[i]] <- textGrob(label,
                           rot = labels$rot[data$labels_par_id],
                           gp = labels$gp[data$labels_par_id],
                           name = paste0("tag.label.", data$labels_par_id),
                           vp = viewport(layout.pos.row = i + j))
    j <- j + 1
  }

  if (do_quantities) {
    i <- i + 1
    grobs[[i]] <- textGrob(quantity,
                           rot = quantities$rot[data$others_par_id],
                           gp = quantities$gp[data$others_par_id],
                           name = paste0("tag.quantity.", data$others_par_id),
                           vp = viewport(layout.pos.row = i + j))
    j <- j + 1
  }

  if (do_percentages) {
    i <- i + 1
    grobs[[i]] <- textGrob(percentage,
                           rot = percentages$rot[data$others_par_id],
                           gp = percentages$gp[data$others_par_id],
                           name = paste0("tag.percentage.", data$others_par_id),
                           vp = viewport(layout.pos.row = i + j))
    j <- j + 1
  }

  # setup heights for the layout of the tags
  ind <- seq(1, n_rows, by = 2)

  heights <- unit(double(n_rows), "null")

  heights[ind] <- unit(1, "grobheight", grobs)

  if (n_grobs > 1) {
    pad_ind <- seq(2, n_rows, by = 2)
    heights[pad_ind] <- eulerr_options()$padding
  }

  parent_vp <- viewport(x = x, y = y,
                        default.units = "native",
                        layout = grid.layout(nrow = n_rows, ncol = 1,
                                             heights = heights))

  grobTree(children = grobs, vp = parent_vp)
}


#' Grobify Euler objects
#'
#' @param x geometry data
#' @param fills fills params
#' @param edges edges params
#' @param labels labels params
#' @param quantities quantities params
#' @param number current diagram number
#' @param centers data.frame of data for labels and quantities
#'
#' @return A [grid::gList()] is returned.
#' @keywords internal
setup_grobs <- function(x,
                        fills,
                        edges,
                        labels,
                        quantities,
                        percentages,
                        centers,
                        number) {
  data_edges <- x$edges
  data_fills <- x$fills
  data_tags <- x$centers
  fitted <- x$fitted.values
  empty_sets <- x$empty_sets
  empty_subsets <- x$empty_subsets

  do_tags <- !is.null(data_tags)
  do_edges <- !is.null(data_edges)
  do_fills <- !is.null(data_fills)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)
  do_percentages <- !is.null(percentages)

  n_e <- NROW(x$ellipses)
  n_id <- 2L^n_e - 1L
  id <- bit_indexr(n_e)

  #edges
  if (do_edges) {
    # edges
    if (is.null(data_edges$x)) {
      edges_grob <- grid::nullGrob()
    } else {
      edges_grob <- grid::polylineGrob(data_edges$x,
                                       data_edges$y,
                                       id.lengths = data_edges$id.lengths,
                                       default.units = "native",
                                       name = "edges.grob",
                                       gp = edges$gp[which(!empty_sets)])
    }
  }

  # fills
  if (do_fills) {
    if (n_e == 0) {
      fills_grob <- grid::nullGrob()
    } else if (n_e == 1) {
      fills_grob <- grid::gList(grid::polygonGrob(
        data_fills[[1]]$x,
        data_fills[[1]]$y,
        default.units = "native",
        name = "fills.grob",
        gp = fills$gp[which(!empty_subsets)[1L]]
      ))
    } else {
      fills_grob <- vector("list", n_id)
      fill_id <- seq_len(n_id)

      for (i in seq_len(n_id)) {
        if (is.null(data_fills[[i]])) {
          fills_grob[[i]] <- grid::nullGrob(name = paste0("fills.grob.", i))
        } else
          fills_grob[[i]] <- grid::pathGrob(
            data_fills[[i]]$x,
            data_fills[[i]]$y,
            id.lengths = data_fills[[i]]$id.lengths,
            default.units = "native",
            name = paste0("fills.grob.", i),
            gp = fills$gp[which(!empty_subsets)][fill_id[i]]
          )
      }
      fills_grob <- do.call(grid::gList, fills_grob)
    }
  }

  do_tags <- do_quantities || do_labels || do_percentages

  # labels
  if (do_tags) {
    tags_grob <- gList()

    for (i in seq_len(NROW(data_tags))) {
      tags_grob[[i]] <- setup_tag(data_tags[i, ],
                                  labels,
                                  quantities,
                                  percentages)
    }
  }

  grid::grobTree(if (do_fills) fills_grob,
                 if (do_edges) edges_grob,
                 if (do_tags) tags_grob,
                 name = paste0("diagram.grob.", number))
}

#' @rdname plot.euler
#' @export
plot.venn <- function(x,
                      fills = TRUE,
                      edges = TRUE,
                      legend = FALSE,
                      labels = identical(legend, FALSE),
                      quantities = TRUE,
                      percentages = FALSE,
                      strips = NULL,
                      main = NULL,
                      n = 200L,
                      ...)
{
  NextMethod("plot", ..., quantities = quantities)
}

#' Print (plot) Euler diagram
#'
#' This function is responsible for the actual drawing of
#' `'eulergram'` objects created through [plot.euler()]. [print.eulergram()]
#' is an alias for [plot.eulergram()], which has been provided so that
#' [plot.euler()] gets called automatically.
#'
#' @param x an object of class `'eulergram'`, usually the output of
#'   [plot.euler()]
#' @param newpage if `TRUE`, opens a new page via [grid.newpage()] to draw on
#' @param ... ignored
#' @return A plot is drawn on the current device using [grid::Grid()] graphics.
#' @export
plot.eulergram <- function(x, newpage = TRUE, ...) {
  if (isTRUE(newpage))
    grid::grid.newpage()
  grid::grid.draw(x)
}

#' @rdname plot.eulergram
#' @export
print.eulergram <- function(x, ...) {
  graphics::plot(x, ...)
}

