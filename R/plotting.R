#' Plot Euler diagrams
#'
#' Plot Euler diagrams with trellis graphics from \pkg{lattice}. This function
#' calls \code{\link[lattice]{xyplot}} under the hood, allowing plots of
#' both individual euler diagrams as well as grids of diagrams
#' in case the \code{by} argument was used in the call to \code{\link{euler}}.
#'
#' If \code{fill} is left blank, \pkg{eulerr} chooses color palettes based on
#' the number of sets and tries to provide palettes adapted to color vision
#' deficiencie based on functionality from \pkg{qualpalr}.
#'
#' Several additional arguments can be handed down to
#' \code{\link[lattice]{xyplot}} via \ldots but be advised that this might cause
#' undesired conflicts with arguments that are provided inside this function.
#' It may be safer to use \code{\link[stats]{update}} to provide further
#' arguments instead.
#'
#' @param x Euler diagram specification from \pkg{eulerr}
#' @param fill Colors to fill circles with.
#' @param fill_opacity Opacity of fill colors.
#' @param border Border color.
#' @param lty Line type(s) for circles as a integer or character vector. (See
#'   \code{\link[grid]{gpar}}.)
#' @param lwd Line weight(s) for circles as a numeric vector. (See
#'   \code{\link[grid]{gpar}}.)
#' @param fontface Fontface as a integer or character vector. (See
#'   \code{\link[grid]{gpar}}.)
#' @param cex The relative size of text as a numeric vector. (See
#'   \code{\link[grid]{gpar}}.)
#' @param labels Labels for the sets as a character vector.
#' @param key Set to \code{TRUE} to automatically generate a legend or input a
#'   list to manually construct the legend (please see the entry
#'   for \code{auto.key} in \code{\link[lattice]{xyplot}}). If \code{FALSE},
#'   labels will be automatically generated inside the sets.
#' @param counts Set to \code{TRUE} to label set combinations with counts from
#'   the input.
#' @param main Title of the plot.
#' @param layout Specifies the layout for the trellis panels if \code{by} was
#'   used in the call to \code{\link{euler}}. This argument cannot be used
#'   in conjunction with \code{outer_strips}, which will be forced to
#'   \code{FALSE} if \code{layout} is not \code{NULL}.
#' @param outer_strips Set to \code{TRUE} to put second level factors on the
#'   left margin of the trellis plot. (This argument is only considered if there
#'   are 2 factors specified to \code{by} in the call to \code{\link{euler}}.)
#' @param \dots Arguments to pass to \code{\link[lattice]{xyplot}}.
#'
#' @return An object of class \code{trellis} from \pkg{lattice}, which has
#' \code{print} (plots the object and is called by default) and \code{update}
#' methods.
#'
#' @seealso \code{\link[lattice]{xyplot}}, \code{\link[grid]{gpar}},
#'   \code{\link[grid]{grid.circle}}, \code{\link[lattice]{panel.xyplot}},
#'   \code{\link{euler}}, \code{\link[qualpalr]{qualpal}}
#'
#' @examples
#' fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))
#' plot(fit, fill_opacity = 0.7)
#'
#' # Change to italic roman font, remove borders and switch colors
#' plot(fit, fill = c("dodgerblue4", "darkgoldenrod1"), lwd = 0,
#'      fontface = "italic")
#'
#' # Add counts to the plot
#' plot(fit, counts = TRUE)
#'
#' # Add a custom legend and retain counts
#' plot(fit, counts = TRUE, key = list(space = "bottom", columns = 2))
#'
#' # Plot without fills and distinguish sets with border types instead
#' plot(fit, lty = c("solid", "dotted"), fill = "transparent", cex = 2,
#'      fontface = 2, labels = c("foo", "bar"))
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
#' plot(e_grid, key = TRUE)
#'
#' # We can modify the grid layout as well
#' plot(e_grid, layout = c(1, 4))
#'
#' @export
plot.euler <- function(x,
                       fill = NULL,
                       fill_opacity = 0.4,
                       border = "black",
                       lty = 1,
                       lwd = 1,
                       fontface = "plain",
                       cex = 1,
                       labels = NULL,
                       key = FALSE,
                       counts = FALSE,
                       main = NULL,
                       layout = NULL,
                       outer_strips = TRUE,
                       ...) {
  # Assertations
  assertthat::assert_that(
    assertthat::is.number(fill_opacity),
    assertthat::is.number(cex),
    assertthat::is.flag(key) | is.list(key)
  )

  ll <- list(...)

  # Add default key if none was specified
  if (is.logical(key)) {
    if (key) {
      key <- list(points = FALSE, rectangles = TRUE)
      if (!is.null(labels)) {
        key[["text"]] <- labels
      }
    }
  } else {
    if (is.null(key[["points"]])) key[["points"]] <- FALSE
    if (is.null(key[["rectangles"]])) key[["rectangles"]] <- TRUE
  }
  ll$auto.key <- key

  # Add qualitative color palette if none was given
  if (is.null(fill)) {
    if (inherits(x, "by")) {
      fill <- fill_color(n = nrow(x[[1]][["coefficients"]]))
    } else {
      fill <- fill_color(n = nrow(x[["coefficients"]]))
    }
  }

  # Apply opacity to color palette
  fill <- grDevices::adjustcolor(fill, fill_opacity)

  ll$aspect <- "iso"
  ll$main <- main
  ll$fontface <- fontface
  ll$cex <- cex
  ll$layout <- layout
  ll$ylab <- ""
  ll$xlab <- ""
  if (is.null(ll$prepanel))
    ll$prepanel <- prepanel_euler
  if (is.null(ll$groups))
    ll$groups <- quote(sets)

  # Remove axes unless the user wants them
  if (is.null(ll$scales)) {
    ll$scales <- list(x = list(draw = FALSE), y = list(draw = FALSE))
  } else {
    if (is.null(ll$scales$x)) {
      ll$scales$x <- list(draw = FALSE)
    } else if (is.null(ll$scales$x$draw)) {
      ll$scales$x$draw <- FALSE
    }
    if (is.null(ll$scales$y)) {
      ll$scales$y <- list(draw = FALSE)
    } else if (is.null(ll$scales$y$draw)) {
      ll$scales$y$draw <- FALSE
    }
  }

  if (is.null(ll$par.settings))
    ll$par.settings <- list()

  if (is.null(ll$par.settings$superpose.polygon$col))
    ll$par.settings$superpose.polygon$col <- fill
  ll$par.settings$superpose.polygon$lty <- lty
  ll$par.settings$superpose.polygon$lwd <- lwd
  ll$par.settings$superpose.polygon$border <- border

  # Turn off the plot area frame unless it is required.
  if (!inherits(x, "by"))
    if (is.null(ll$par.settings$axis.line$col))
      ll$par.settings$axis.line$col <- 0

  if (inherits(x, "by")) {
    d  <- dim(x)
    dn <- dimnames(x)
    euler_list <- label_data <- data.frame()

    for (i in seq_along(x)) {
      ii <- i - 1
      for (j in seq_along(dn)) {
        iii <- ii %% d[j] + 1
        ii <- ii %/% d[j]
        if (j == 1) {
          title <- dn[[j]][iii]
        } else {
          title <- cbind(title, dn[[j]][iii])
        }
      }

      euler_list <- rbind(euler_list,
                          data.frame(x[[i]][["coefficients"]], title))

      if (any(!is.list(key), counts)) {
        label_data <- rbind(
          label_data,
          data.frame(position_labels(x[[i]], counts, labels, key), i)
        )
      }
    }

    if (length(dn) == 1) {
      colnames(euler_list) <- c("x", "y", "r", "v1")
    } else {
      colnames(euler_list) <- c("x", "y", "r", "v1", "v2")
    }

    euler_list$sets <- rownames(x[[i]][["coefficients"]])

    ll$r <- euler_list$r
    ll$data <- euler_list
    ll$label_data <- label_data
    ll$panel <- function(x, y, r, label_data, subscripts, ...) {
      panel_circles(x, y, r, subscripts, ...)
      if (!is.list(key) | counts) {
        panel_labels(x, y, label_data, ...)
      }
    }

    if (length(dn) == 1) {
      ll$x <- stats::formula(y ~ x | v1)

      do.call(lattice::xyplot, ll)

    } else {
      ll$x <- stats::formula(y ~ x | v1 + v2)

      if (outer_strips & is.null(layout)) {
        latticeExtra::useOuterStrips(do.call(lattice::xyplot, ll))
      } else {
        do.call(lattice::xyplot, ll)
      }
    }
  } else {
    dd <- data.frame(x[["coefficients"]], rownames(x[["coefficients"]]))
    colnames(dd) <- c("x", "y", "r", "sets")

    ll$x <- stats::formula(y ~ x)
    ll$r <- dd$r
    ll$data <- dd
    ll$label_data <- position_labels(x, counts, labels, key)
    ll$panel <- function(x, y, r, label_data, subscripts, ...) {
      panel_circles(x, y, r, subscripts, ...)
      if (any(!is.list(key), counts)) {
        lattice::panel.text(labels = label_data$labels,
                            x = label_data$x,
                            y = label_data$y, ...)
      }
    }

    do.call(lattice::xyplot, ll)
  }
}

prepanel_euler <- function(x, y, r, subscripts, ...) {
  list(xlim = range(x + r[subscripts], x - r[subscripts]),
       ylim = range(y + r[subscripts], y - r[subscripts]))
}

panel_circles <- function(x, y, r, subscripts, ...) {
  grid::grid.circle(
    x = x,
    y = y,
    r = r[subscripts],
    default.units = "native",
    gp = grid::gpar(
      fill = lattice::trellis.par.get()$superpose.polygon$col,
      lty = lattice::trellis.par.get()$superpose.polygon$lty,
      border = lattice::trellis.par.get()$superpose.polygon$border,
      lwd = lattice::trellis.par.get()$superpose.polygon$lwd,
      alpha = lattice::trellis.par.get()$superpose.polygon$alpha
    )
  )
}

panel_labels <- function(x, y, data, ...) {
  dat <- data[data$i == lattice::packet.number(), ]
  lattice::panel.text(labels = dat$labels, x = dat$x, y = dat$y, ...)
}

# Compute coordinates for labels ------------------------------------------

position_labels <- function(fit, counts, labels, key) {
  x <- fit[["coefficients"]][, 1]
  y <- fit[["coefficients"]][, 2]
  r <- fit[["coefficients"]][, 3]

  n <- length(r)
  text_x <- text_y <- double(n)

  n_samples <- 500L
  seqn  <- seq.int(0L, n_samples - 1, 1L)
  theta <- seqn * pi * (3L - sqrt(5L))
  rad   <- sqrt(seqn / n_samples)
  px    <- rad * cos(theta)
  py    <- rad * sin(theta)

  # In case the user asks for counts, compute locations for these
  if (counts) {
    not_zero <- fit[["fitted.values"]] > (.Machine$double.eps) ^ 0.25
    xx_labels <- yy_labels <- double(length(not_zero))
    xx_labels[] <- NA
    yy_labels[] <- NA
    id <- bit_index(n)
    mode(id) <- 'logical'
    set_labels <- character(length(not_zero))
  }

  for (i in seq_along(r)) {
    xs <- px * r[i] + x[i]
    ys <- py * r[i] + y[i]
    in_which <- find_sets_containing_points(cbind(xs, ys), x, y, r)

    if (counts) {
      which_area <- which(not_zero & id[, i])
      set_labels[which_area[1]] <-
        paste0(ifelse(is.null(labels), names(r)[i], labels[i]), "\n")

      for (j in 1:nrow(id)) {
        idx <- id[j, ]
        if (all(is.na(xx_labels[j]), not_zero[j], idx[i])) {
          locs_labels <- apply(in_which, 2, function(x) all(x == idx))
          xx_lab <- xs[locs_labels]
          yy_lab <- ys[locs_labels]
          lab_cplist <- mapply(dist_point_circle, xx_lab, yy_lab,
                               MoreArgs = list(h = x, k = y, r = r),
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)

          if (assertthat::not_empty(lab_cplist)) {
            lab_cp <- matrix(unlist(lab_cplist, use.names = FALSE), nrow = n)
            if (ncol(lab_cp) > 0) {
              labmax <- col_mins(lab_cp)
              xx_labels[j] <- xx_lab[labmax]
              yy_labels[j] <- yy_lab[labmax]
            }
          }
        }
      }
    } else {
      locs <- colSums(in_which)
      outskirts <- locs == min(locs)
      xx <- xs[outskirts]
      yy <- ys[outskirts]
      cp_list <- mapply(dist_point_circle, xx , yy,
                        MoreArgs = list(h = x, k = y, r = r),
                        SIMPLIFY = FALSE, USE.NAMES = FALSE)
      cp <- matrix(unlist(cp_list, use.names = FALSE), nrow = n)
      dipmax <- col_mins(cp)
      text_x[i] <- xx[dipmax]
      text_y[i] <- yy[dipmax]
    }
  }
  if (counts) {
    labels_x <- xx_labels
    labels_y <- yy_labels
    if (is.list(key)) {
      labels_text <- fit[["original.values"]]
    } else {
      labels_text <- paste0(set_labels, fit[["original.values"]])
    }
  } else {
    labels_x <- text_x
    labels_y <- text_y
    if (is.null(labels)) {
      labels_text <- names(r)
    } else {
      labels_text <- labels
    }
  }
  data.frame(x = labels_x, y = labels_y, labels = labels_text)
}

# Set up qualitative color palette ----------------------------------------

fill_color <- function(n) {
  palettes[[n]]
}


# Convencience function to set list options -------------------------------

ifnulldo <- function(x, nullobj = x, do) {
  if (is.null(nullobj)) {
    x <<- do
  }
}
