#' eulerr plots
#'
#' Plot Euler diagrams from eulerr using base R graphics.
#'
#' If no color is specified to \code{polygon_args}, fills will be colored
#' using a predefined palette taken from \pkg{qualpal}.
#'
#' @param x Specifications for a Euler diagram in the form of a object of class
#'   \code{'eulerr'}
#' @param fill_opacity Opacity for the fill colors between 0 and 1. Values
#'   beyond this range will be clamped to [0, 1] without warning.
#' @param polygon_args Arguments for \code{\link[graphics]{polygon}},
#'   which is used to draw the circles.
#' @param text_args Arguments for \code{\link[graphics]{text}},
#'   which is used to draw the text.
#' @param mar Margins for the plot area, set via\code{par()[["mar"]]}.
#' @param \dots Arguments for \code{\link[graphics]{plot}} (that draws the plot
#'   area).
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}},
#'   \code{\link[graphics]{text}} \code{\link{eulerr}}
#' @examples
#' fit <- eulerr(c(A = 10, B = 5, "A&B" = 3))
#' plot(fit, fill_opacity = .7)
#'
#' # Change to italic roman font, remove borders and switch colors
#' plot(fit,
#'      polygon_args = list(col = c("dodgerblue4", "darkgoldenrod1"),
#'                          border = "transparent"),
#'      text_args = list(font = 8))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit,
#'      polygon_args = list(lty = c("solid", "dotted"), col = "transparent"),
#'      text_args = list(cex = 2, font = 2))
#'
#' @export
#' @import assertthat

plot.eulerr <- function(x, fill_opacity = 0.4, polygon_args = list(),
                        text_args = list(), mar = c(2, 2, 2, 2), ...) {
  assert_that(
    length(mar) == 4,
    is.numeric(mar),
    is.number(fill_opacity),
    is.list(polygon_args),
    is.list(text_args)
  )
  old_mar <- graphics::par(no.readonly = TRUE)[["mar"]]
  on.exit(graphics::par(mar = old_mar))
  graphics::par(mar = mar)

  X <- stats::coef(x)[, 1L]
  Y <- stats::coef(x)[, 2L]
  r <- stats::coef(x)[, 3L]

  plot_args <- list(...)
  if(is.null(plot_args$x)) plot_args$x <- double(0L)
  plot_args$xlab <- ""
  plot_args$ylab <- ""
  if(is.null(plot_args$axes)) plot_args$axes <- FALSE
  if(is.null(plot_args$xlim)) plot_args$xlim <- range(c(X + r, X - r))
  if(is.null(plot_args$ylim)) plot_args$ylim <- range(c(Y + r, Y - r))
  if(is.null(plot_args$asp)) plot_args$asp <- 1L

  do.call(graphics::plot, plot_args)

  g <- seq(0L, 2L * pi, length = 500L)
  x_coords <- double(0L)
  y_coords <- double(0L)
  for (i in seq_along(X)) {
    x_coords <- c(x_coords, r[i] * cos(g) + X[i], NA)
    y_coords <- c(y_coords, r[i] * sin(g) + Y[i], NA)
  }

  # Use color palette from qualpalr if none was specified.
  if (is.null(polygon_args$col)) {
    polygon_args$col <- c("#6ACB69", "#C96CC6", "#76AACE", "#CF9D74", "#E4CCDD",
                          "#C0EADF", "#6A71CC", "#E0E1A8", "#C86D7A", "#AFA1DD",
                          "#C19DA0", "#77BDA5", "#C6D4E8", "#E6CCB9", "#E1A7DC",
                          "#B0C2A3", "#C8B778", "#8BCDD7", "#9F79CC", "#7391CE")
  }

  polygon_args$col <-
    grDevices::adjustcolor(polygon_args$col, alpha = fill_opacity)
  polygon_args$x <- x_coords
  polygon_args$y <- y_coords

  do.call(graphics::polygon, polygon_args)

  # Find good positions for the text centers
  text_x <- double(length(X))
  text_y <- double(length(Y))

  # Pick a text center location by filling each circle with points and then
  # picking a point in the area of highest density

  n <- 499L
  theta <- seq.int(0L, n, 1L) * pi * (3L - sqrt(5L))
  rad   <- sqrt(0L:n) / sqrt(n)
  px    <- rad * cos(theta)
  py    <- rad * sin(theta)
  max_rad <- max(rad)

  for (i in seq_along(r)) {
    xs <- px * (r[i] / max_rad) + X[i]
    ys <- py * (r[i] / max_rad) + Y[i]

    locs <- colSums(find_sets_containing_points(cbind(xs, ys), X, Y, r))

    tip <- table(locs)

    if (any(tip) > 40L) {
      large_enough <- which(tip > 40L)[1L]
    } else if (any(tip) > 30L) {
      large_enough <- which(tip > 30L)[1L]
    } else {
      large_enough <- which(tip > 5L)[1L]
    }

    large_enough <- which(table(locs) > 30L)[1L]

    outskirts <- locs == as.numeric(names(large_enough))
    xx <- xs[outskirts]
    yy <- ys[outskirts]

    dens <- MASS::kde2d(xx, yy, n = 51L)

    mind <- which(dens$z == max(dens$z), arr.ind = TRUE, useNames = FALSE)
    text_x[i] <- dens$x[mind[, 1L]]
    text_y[i] <- dens$y[mind[, 2L]]
  }

  text_args$x      <- text_x
  text_args$y      <- text_y
  text_args$labels <- names(r)

  do.call(graphics::text, text_args)
}

#' Plot eulerr plot grid
#'
#' Plot a grid of eulerr plots.
#'
#' @param x A grid of euler diagrams of class \code{eulerr_grid} produced by
#'   \code{\link{eulerr}}.
#' @param main Titles for the euler plots. If not provided, uses grouping
#'   variables from \code{\link{eulerr}}.
#' @param mfrow Number of rows and columns in the grid as a vector of two
#'   integers: \code{c(columns, rows)}.
#' @param \dots Arguments to pass forward to \code{\link{plot.eulerr}}.
#' @seealso \code{\link{plot.eulerr}}, \code{\link{eulerr}},
#'   \code{\link[graphics]{par}}
#' @examples
#' dat <- data.frame(
#'   A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' e_grid <- eulerr(dat[, 1:2], by = dat[, 3:4])
#' plot(e_grid)
#'
#' # We can provide custom titles for our diagrams
#'
#' plot(e_grid, main = c("A", "B", "C", "D"))
#'
#' # and use any options that plot.eulerr takes
#'
#' plot(e_grid, polygon_args = list(col = "transparent"))
#'
#' # It is also possible to change grid layout
#'
#' plot(e_grid, mfrow = c(1, 4))
#'
#' @import assertthat
#' @export

plot.eulerr_grid <- function(x, main, mfrow, ...) {
  if (!missing(main)) {
    assert_that(is.character(main), length(main) == length(x))
  }

  if (missing(mfrow)) {
    lsq <- sqrt(length(x))
    n <- floor(lsq)
    m <- ceiling(lsq)
  } else {
    assert_that(length(mfrow) == 2, is.numeric(mfrow))
    n <- mfrow[1L]
    m <- mfrow[2L]
  }

  old_par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(n, m))
  on.exit(graphics::par(old_par))

  d  <- dim(x)
  dn <- dimnames(x)

  for (i in seq_along(x)) {
    ii <- i - 1L
    if (missing(main)) {
      for (j in seq_along(dn)) {
        iii <- ii %% d[j] + 1L
        ii <- ii %/% d[j]
        if (j == 1L) {
          title <- dn[[j]][iii]
        } else {
          title <- paste(title, dn[[j]][iii], sep = ", ")
        }
      }
      plot.eulerr(x[[i]], main = title, ...)
    } else {
      plot.eulerr(x[[i]], main = main[i], ...)
    }
  }
}
