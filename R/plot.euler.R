#' Plot area-proportional Euler diagrams
#'
#' Hello
#'
#' @param x an object of class `'euler'`, generated from [euler()]
#' @param mode "`split`", the default, splits up the diagram into individual
#'   polygons and blends the colors of the overlapping shapes using
#'   color averaging in the CIELAB color space. "`overlay`" superposes
#'   sets and should be used in conjunction with a suitable `fill_alpha` value.
#' @param legend draw a legend
#' @param labels draw labels, either a logical scalar, a character vector, or a
#'   list
#' @param quantities draw quantities, either a logical scalar or a list
#' @param edges draw edges, either a logical or a list
#' @param fills draw fills, either a logical or a list
#' @param n number of vertices for the ellipses
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
#' @return Provides an `'euler_diagram'` object, which is a description of
#'   the diagram to be drawn. [print.euler_diagram()] does the actual plotting
#'   of the diagram and is usually called automatically when the object is
#'   printed to screen.
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
#' plot(fit, quantities = TRUE, legend = list(labels = c("foo", "bar")))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit, fills = FALSE, edges = list(lty = 1:2))
#'
plot.euler <- function(x,
                       mode = c("split", "overlay"),
                       legend = FALSE,
                       fills = TRUE,
                       edges = TRUE,
                       labels = identical(legend, FALSE),
                       quantities = FALSE,
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

  mode <- match.arg(mode)

  # extract parameters from euler diagram
  # if (is_by) {
  #   dd <- do.call(rbind, lapply(x, "[[", "coefficients"))
  #   orig <- do.call(cbind, lapply(x, "[[", "original.values"))
  #   fitted <- do.call(cbind, lapply(x, "[[", "fitted.values"))
  #   if (isTRUE(labels))
  #     labels <- rownames(x[[1]]$coefficients)
  #   else if (is.logical(labels))
  #     labels <- NULL
  # } else {
    dd <- x$coefficients
    orig <- x$original.values
    fitted <- x$fitted.values
  # }

  setnames <- rownames(dd)

  h <- dd$h
  k <- dd$k
  a <- dd$a
  b <- dd$b
  phi <- dd$phi

  n_e <- length(h)
  n_id <- 2^n_e - 1
  id <- eulerr:::bit_indexr(n_e)

  e <- eulerr:::ellipse(h, k, a, b, phi, n = n)
  e_x <- c(lapply(e, "[[", "x"), recursive = TRUE)
  e_y <- c(lapply(e, "[[", "y"), recursive = TRUE)

  limits <- eulerr:::get_bounding_box(h, k, a, b, phi)

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
      fills$data <- list(x = e_x, y = e_y, id.lengths = rep.int(n, n_e))
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
  if (is.character(labels))
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

#' Print (plot) Euler diagram
#'
#' This method for [print()] is responsible for the actual plotting of
#' `'euler_diagram'` objects created through [plot.euler()] and is usually
#' called automatically after an `'euler'` object is printed on the screen.
#'
#' @param x an object of class `'euler_diagram'` -- usually the output of
#'   [plot.euler()].
#' @param ... ignored
#'
#' @return A plot is drawn on the current device using [grid::Grid()].
#' @export
#'
#' @examples
#' p <- plot(euler(c(A = 1, B = 3, "A&B" = 0.5)))
#' print(p)
print.euler_diagram <- function(x, ...) {
  ellipses <- x$ellipses
  mode <- x$mode
  legend <- x$legend
  labels <- x$labels
  edges <- x$edges
  fills <- x$fills
  quantities <- x$quantities
  limits <- x$limits

  n_e <- nrow(ellipses)
  n_id <- 2L^n_e - 1L
  id <- eulerr:::bit_indexr(n_e)

  xlim <- grDevices::extendrange(limits$xlim, f = 0.01)
  ylim <- grDevices::extendrange(limits$ylim, f = 0.01)
  xrng <- abs(xlim[1L] - xlim[2L])
  yrng <- abs(ylim[1L] - ylim[2L])
  ar <- xrng/yrng

  #edges
  if (is.list(edges)) {
    # edges
    edges_grob <- grid::polygonGrob(edges$data$x,
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

  euler_vp <- grid::viewport(
    width = grid::unit(min(1, diff(xlim)/diff(ylim)), "snpc"),
    height = grid::unit(min(1, diff(ylim)/diff(xlim)), "snpc"),
    xscale = xlim,
    yscale = ylim
  )

  euler_grob <- grid::grobTree(fills_grob,
                               edges_grob,
                               labels_grob,
                               quantities_grob)
  if (is.list(legend)) {
    legend_grob <- do.call(grid::legendGrob, legend)
  } else {
    legend_grob <- grid::nullGrob()
  }

  grob_layout <- grid::grid.layout(
    1,
    3,
    widths = grid::unit(c(1, 1, 1),
                        c("null", "lines","grobwidth"),
                        list(NULL, NULL, legend_grob)),
    height = grid::unit(1/ar, "null"),
    respect = TRUE,
    just = "center"
  )

  grid::grid.newpage()
  grid::pushViewport(grid::vpTree(
    grid::viewport(layout = grob_layout, name = "base"),
    grid::vpList(grid::viewport(layout.pos.row = 1,
                                layout.pos.col = 1,
                                xscale = xlim,
                                yscale = ylim,
                                name = "one"),
                 grid::viewport(layout.pos.row = 1,
                                layout.pos.col = 3,
                                width = grid::grobWidth(legend_grob),
                                name = "two"))
  ))
  grid::upViewport()
  grid::downViewport("one")
  grid::grid.draw(euler_grob)
  grid::upViewport()
  grid::downViewport("two")
  grid::grid.draw(legend_grob)
  grid::popViewport(0)

  grid::popViewport(n = 0)
}
