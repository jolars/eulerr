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
  assert_that(inherits(x, "eulerr"))
  assert_that(length(mar) == 4)
  assert_that(is.numeric(mar))
  assert_that(is.numeric(fill_opacity))
  assert_that(is.list(polygon_args))
  assert_that(is.list(text_args))

  old_mar <- graphics::par(no.readonly = TRUE)[["mar"]]
  on.exit(graphics::par(mar = old_mar))
  graphics::par(mar = mar)

  X <- stats::coef(x)[, 1]
  Y <- stats::coef(x)[, 2]
  r <- stats::coef(x)[, 3]

  plot_args <- list(...)
  if(is.null(plot_args$x)) plot_args$x <- double(0)
  plot_args$xlab <- ""
  plot_args$ylab <- ""
  if(is.null(plot_args$axes)) plot_args$axes <- FALSE
  if(is.null(plot_args$xlim)) plot_args$xlim <- range(c(X + r, X - r))
  if(is.null(plot_args$ylim)) plot_args$ylim <- range(c(Y + r, Y - r))
  if(is.null(plot_args$asp)) plot_args$asp <- 1

  do.call(graphics::plot, plot_args)

  g <- seq(0, 2 * pi, length = 500)
  x_coords <- double(0)
  y_coords <- double(0)
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

  # Pick a text center location by filling each circle with a quasirandom
  # point sequence and finding the center of the points that belong to the
  # least number of other circles
  rpoints <- randtoolbox::sobol(300, dim = 2, scrambling = 2)
  u <- 2 * pi * (rpoints[, 1] - 1) + 2 * pi
  v <- sqrt(rpoints[, 2])
  for (i in seq_along(r)) {
    rs <- r[i] * (v - 1) + r[i]
    xs <- X[i] + rs * cos(u)
    ys <- Y[i] + rs * sin(u)

    locs <-
      colSums(apply(cbind(xs, ys), 1, find_sets_containing_points, X, Y, r))

    outskirts <- which(locs == min(locs))
    text_x[i] <- stats::median(xs[outskirts], na.rm = TRUE)
    text_y[i] <- stats::median(ys[outskirts], na.rm = TRUE)
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
#' plot(f, mfrow = c(1, 4))
#'
#' @import assertthat
#' @export

plot.eulerr_grid <- function(x, main, mfrow, ...) {
  assert_that(inherits(x, "eulerr_grid"))

  if (missing(mfrow)) {
    lsq  <- sqrt(length(x))
    n <- floor(lsq)
    m <- ceiling(lsq)
  } else {
    assert_that(length(mfrow) == 2)
    assert_that(is.numeric(mfrow))
    n <- mfrow[1]
    m <- mfrow[2]
  }

  old_par <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(n, m))
  on.exit(graphics::par(old_par))

  if (!missing(main)) {
    assert_that(is.character(main))
    assert_that(length(main) == length(x))
  }

  d  <- dim(x)
  dn <- dimnames(x)

  for (i in seq_along(x)) {
    ii <- i - 1L
    if (missing(main)) {
      for (j in seq_along(dn)) {
        iii <- ii%%d[j] + 1L
        ii <- ii%/%d[j]
        if (j == 1) {
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