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
#' @param counts Display counts in the diagram.
#' @param \dots Arguments for \code{\link[graphics]{plot}} (that draws the plot
#'   area).
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}},
#'   \code{\link[graphics]{text}} \code{\link{eulerr}}
#'
#' @examples
#' fit <- eulerr(c(A = 10, B = 5, "A&B" = 3))
#' plot(fit, fill_opacity = .7)
#'
#' # Change to italic roman font, remove borders and switch colors
#' plot(fit,
#'      polygon_args = list(col = c("dodgerblue4", "darkgoldenrod1"),
#'                          border = "transparent"),
#'      text_args = list(font = 3))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit,
#'      polygon_args = list(lty = c("solid", "dotted"), col = "transparent"),
#'      text_args = list(cex = 2, font = 2))
#'
#' @export

plot.eulerr <- function(x, fill_opacity = 0.4, polygon_args = list(),
                        text_args = list(), mar = c(2, 2, 2, 2), counts = FALSE,
                        ...) {
  assertthat::assert_that(
    length(mar) == 4,
    is.numeric(mar),
    assertthat::is.number(fill_opacity),
    is.list(polygon_args),
    is.list(text_args)
  )

  old_mar <- graphics::par()[["mar"]]
  on.exit(graphics::par(mar = old_mar))
  graphics::par(mar = mar)

  X <- x[["coefficients"]][, 1]
  Y <- x[["coefficients"]][, 2]
  r <- x[["coefficients"]][, 3]

  n <- length(X)

  plot_args <- list(...)
  plot_args$x <- 0
  plot_args$type <- "n"
  plot_args$xlab <- ""
  plot_args$ylab <- ""
  if (is.null(plot_args$axes)) plot_args$axes <- FALSE
  if (is.null(plot_args$xlim)) plot_args$xlim <- range(c(X + r, X - r))
  if (is.null(plot_args$ylim)) plot_args$ylim <- range(c(Y + r, Y - r))
  if (is.null(plot_args$asp)) plot_args$asp <- 1

  do.call(graphics::plot, plot_args)

  g <- seq(0L, 2L * pi, length = 500L)
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
  text_x <- double(n)
  text_y <- double(n)

  # Place a grid of points across the diagram
  smpl_pts <- expand_grid(seq.int(min(X - r), max(X + r), length.out = 100),
                          seq.int(min(Y - r), max(Y + r), length.out = 100))

  smpl_in <- find_sets_containing_points(smpl_pts, X, Y, r)

  for (i in seq_along(r)) {

    in_curr <- smpl_in[i, ]
    out_curr <- smpl_in[-i, , drop = FALSE]

    candidates <- out_curr[, in_curr, drop = FALSE]
    candsums <- colSums(candidates)

    pick_these <- candsums == min(candsums)

    xx <- smpl_pts[in_curr, 1][pick_these]
    yy <- smpl_pts[in_curr, 2][pick_these]

    pcd <- mapply(dist_point_circle, xx, yy,
                  MoreArgs = list(h = X, k = Y, r = r))

    max_ind <- which.max(pcd[(1:ncol(pcd) - 1) * nrow(pcd) + max.col(t(-pcd))])

    text_x[i] <- xx[max_ind]
    text_y[i] <- yy[max_ind]
  }

  if (counts) {

    no_combos <- choose(n, 1L:n)
    id <- matrix(FALSE, sum(no_combos), n)
    cum_combos <- c(0, cumsum(no_combos)[-n])

    k <- 1
    for (i in cum_combos) {
      permutations <- utils::combn(n, k)
      for (j in 1:(ncol(permutations))) {
        id[i + j, permutations[, j]] <- TRUE
      }
      k <- k + 1
    }

    fpts <- which(rowSums(id) > 1 & x$fitted.values > 0)
    centroid <- matrix(NA, nrow = nrow(id), ncol = 3)

    for (i in fpts) {
      sets_a <- which(id[i, ])

      aa <- colSums(smpl_in[sets_a, ]) == length(sets_a)
      minbb <- colSums(smpl_in[-sets_a, , drop = FALSE])
      bb <- minbb == min(minbb)

      centpoints <- aa & bb

      sets_id <- id[, sets_a]
      sets_ind <- rowSums(sets_id) == ncol(sets_id)
      sets_areas <- x$original.values[sets_ind]

      # Find the areas of all sets with this combination of sets in them
      # in the hierarchy above.
      set_size <- sets_areas[1] - sum(sets_areas[-1])

      centroid[i, ] <- cbind(sum(smpl_pts[centpoints, 1]) / sum(centpoints),
                             sum(smpl_pts[centpoints, 2]) / sum(centpoints),
                             set_size)
    }

    graphics::text(x = centroid[, 1],
                   y = centroid[, 2],
                   labels = centroid[, 3])
  }

  text_args$x <- text_x
  text_args$y <- text_y

  if (counts) {
    text_args$labels <- paste(names(r), "\n", x$original.values[1:length(r)],
                              sep = "")
  } else {
    text_args$labels <- names(r)
  }

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
#' @export

plot.eulerr_grid <- function(x, main, mfrow, ...) {
  if (!missing(main)) {
    assertthat::assert_that(
      is.character(main),
      length(main) == length(x)
    )
  }
  if (missing(mfrow)) {
    lsq <- sqrt(length(x))
    n <- floor(lsq)
    m <- ceiling(lsq)
  } else {
    assertthat::assert_that(
      length(mfrow) == 2,
      is.numeric(mfrow)
    )

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
