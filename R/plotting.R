#' euler plots
#'
#' Plot Euler diagrams from euler using base R graphics.
#'
#' If no color is specified to \code{polygon_args}, fills will be colored
#' using a predefined palette taken from \pkg{qualpal}.
#'
#' @param x Specifications for a Euler diagram in the form of a object of class
#'   \code{'euler'}
#' @param fill_opacity Opacity for the fill colors between 0 and 1. Values
#'   beyond this range will be clamped to [0, 1] without warning.
#' @param polygon_args Arguments for \code{\link[graphics]{polygon}},
#'   which is used to draw the circles.
#' @param text_args Arguments for \code{\link[graphics]{text}},
#'   which is used to draw the text.
#' @param mar Margins for the plot area, set via\code{par()[["mar"]]}.
#' @param counts Plot counts for each unique section of the diagram. Thse are
#'   the values from the original set specification.
#' @param \dots Arguments for \code{\link[graphics]{plot}} (that draws the plot
#'   area).
#' @seealso \code{\link[graphics]{plot}}, \code{\link[graphics]{polygon}},
#'   \code{\link[graphics]{text}} \code{\link{euler}}
#'
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
#' plot(fit, fill_opacity = 0.7)
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
#' # Plot a grid of euler plots
#' dat <- data.frame(
#'   A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
#'   B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
#'   gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
#'   nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
#' )
#'
#' e_grid <- euler(dat[, 1:2], by = dat[, 3:4])
#' plot(e_grid)
#'
#' # We can provide custom titles for our diagrams
#'
#' plot(e_grid, main = c("A", "B", "C", "D"))
#'
#' # and use any options that plot.euler takes
#'
#' plot(e_grid, polygon_args = list(col = "transparent"))
#'
#' # It is also possible to change grid layout
#'
#' plot(e_grid, mfrow = c(1, 4))
#'
#' @export

plot.euler <- function(x,
                       fill_opacity = 0.4,
                       polygon_args = list(),
                       text_args = list(),
                       mar = c(2, 2, 2, 2),
                       counts = FALSE,
                       main,
                       mfrow,
                       ...) {
  # Plot a grid of euler plots
  if (inherits(x, "by")) {
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
        # Recurse and plot each separate euler diagram
        plot(x[[i]], main = title, ...)
      } else {
        plot(x[[i]], main = main[i], ...)
      }
    }

  } else {
    assertthat::assert_that(
      length(mar) == 4,
      is.numeric(mar),
      assertthat::is.number(fill_opacity),
      is.list(polygon_args),
      is.list(text_args)
    )
    graphics::par(mar = mar)

    X <- x[["coefficients"]][, 1L]
    Y <- x[["coefficients"]][, 2L]
    r <- x[["coefficients"]][, 3L]
    n <- length(r)

    plot_args <- list(...)
    if (is.null(plot_args$x))
      plot_args$x <- double(0L)
    if (!missing(main))
      plot_args$main <- main
    if (is.null(plot_args$type))
      plot_args$type <- "n"
    if (is.null(plot_args$axes))
      plot_args$axes <- FALSE
    if (is.null(plot_args$xlim))
      plot_args$xlim <- range(c(X + r, X - r))
    if (is.null(plot_args$ylim))
      plot_args$ylim <- range(c(Y + r, Y - r))
    if (is.null(plot_args$asp))
      plot_args$asp <- 1L
    plot_args$xlab <- ""
    plot_args$ylab <- ""

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
    text_x <- text_y <- double(n)

    # Pick a text center location by filling each circle with points and then
    # picking a point in the area of highest density

    n_samples <- 500L
    seqn  <- seq.int(0L, n_samples - 1, 1L)
    theta <- seqn * pi * (3L - sqrt(5L))
    rad   <- sqrt(seqn / n_samples)
    px    <- rad * cos(theta)
    py    <- rad * sin(theta)

    if (counts) {
      not_zero <- x[["fitted.values"]] > (.Machine$double.eps) ^ 0.25
      xx_labels <- yy_labels <- double(length(not_zero))
      xx_labels[] <- NA
      yy_labels[] <- NA
      id <- bit_index(n)
      mode(id) <- 'logical'
      set_labels <- character(length(not_zero))
    }
    for (i in seq_along(r)) {
      xs <- px * r[i] + X[i]
      ys <- py * r[i] + Y[i]

      in_which <- find_sets_containing_points(cbind(xs, ys), X, Y, r)

      if (counts) {
        which_area <- which(not_zero & id[, i])
        set_labels[which_area[1]] <- paste0(names(r)[i], "\n")

        for (j in 1:nrow(id)) {
          idx <- id[j, ]

          if (all(is.na(xx_labels[j]), not_zero[j], idx[i])) {
            locs_labels <- apply(in_which, 2, function(x) all(x == idx))

            xx_lab <- xs[locs_labels]
            yy_lab <- ys[locs_labels]

            lab_cplist <- mapply(dist_point_circle, xx_lab , yy_lab,
                                 MoreArgs = list(h = X, k = Y, r = r),
                                 SIMPLIFY = FALSE, USE.NAMES = FALSE)

            lab_cp <- matrix(unlist(lab_cplist, use.names = FALSE), nrow = n)

            if (ncol(lab_cp) > 0) {
              labmax <- col_mins(lab_cp)
              xx_labels[j] <- xx_lab[labmax]
              yy_labels[j] <- yy_lab[labmax]
            }
          }
        }
      } else {
        locs <- colSums(in_which)
        outskirts <- locs == min(locs)

        xx <- xs[outskirts]
        yy <- ys[outskirts]

        cp_list <- mapply(dist_point_circle, xx , yy,
                          MoreArgs = list(h = X, k = Y, r = r),
                          SIMPLIFY = FALSE, USE.NAMES = FALSE)

        cp <- matrix(unlist(cp_list, use.names = FALSE), nrow = n)

        dipmax <- col_mins(cp)

        text_x[i] <- xx[dipmax]
        text_y[i] <- yy[dipmax]
      }
    }
    if (counts) {
      text_args$x <- xx_labels
      text_args$y <- yy_labels
      text_args$labels <- paste0(set_labels, x[["original.values"]])
    } else {
      text_args$x <- text_x
      text_args$y <- text_y
      text_args$labels <- names(r)
    }
    do.call(graphics::text, text_args)
  }
}
