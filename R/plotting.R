#' Plot Euler diagrams
#'
#' Plot Euler diagrams using base R graphics.
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
#' @param \dots Arguments for \code{\link[graphics]{plot}}, which is used to draw
#'   the plot area.
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}},
#'   \code{\link[graphics]{text}}
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
#' # Slim down margins and surround plot with a box
#' opar <- par(no.readonly = TRUE)
#' par(mar = c(0.1, 0.1, 0.1, 0.1))
#' plot(fit)
#' box()
#' par(opar)
#'
#' @export
#' @import assertthat

plot.eulerr <- function(x, fill_opacity = 0.4,  polygon_args = list(),
                        text_args = list(), ...) {
  assert_that(inherits(x, "eulerr"))

  X <- x[["circles"]][, 1]
  Y <- x[["circles"]][, 2]
  r <- x[["circles"]][, 3]

  graphics::plot(
    NULL,
    axes = F,
    ann = F,
    xlim = range(c(X + r, X - r)),
    ylim = range(c(Y + r, Y - r)),
    asp = 1,
    ...
  )

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

  polygon_args$col <- grDevices::adjustcolor(polygon_args$col,
                                             alpha = fill_opacity)
  polygon_args$x <- x_coords
  polygon_args$y <- y_coords

  do.call(graphics::polygon, polygon_args)

  # Find good positions for the text centers
  text_x <- double(length(X))
  text_y <- double(length(Y))

  rpoints <- randtoolbox::sobol(100, 2)
  u <- 2 * pi * (rpoints[, 1] - 1) + 2 * pi
  v <- sqrt(rpoints[, 2])
  for (i in seq_along(r)) {
    rs <- r[i] * (v - 1) + r[i]
    xs <- X[i] + rs * cos(u)
    ys <- Y[i] + rs * sin(u)

    locs <- colSums(apply(cbind(xs, ys), 1, find_sets_containing_points,
                          X, Y, r))

    outskirts <- which(locs == min(locs))
    text_x[i] <- mean(xs[outskirts])
    text_y[i] <- mean(ys[outskirts])
  }

  text_args$x <- text_x
  text_args$y <- text_y
  text_args$labels <- names(r)

  do.call(graphics::text, text_args)
}

