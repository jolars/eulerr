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
#' \tabular{lccccccc}{
#'              \tab fills \tab edges \tab labels \tab quantities  \tab strips \tab legend \tab main \cr
#'   col        \tab       \tab x     \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   fill       \tab x     \tab       \tab        \tab             \tab        \tab        \tab      \cr
#'   alpha      \tab x     \tab x     \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   lty        \tab       \tab x     \tab        \tab             \tab        \tab        \tab      \cr
#'   lwd        \tab       \tab x     \tab        \tab             \tab        \tab        \tab      \cr
#'   lex        \tab       \tab x     \tab        \tab             \tab        \tab        \tab      \cr
#'   fontsize   \tab       \tab       \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   cex        \tab       \tab       \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   fontfamily \tab       \tab       \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   lineheight \tab       \tab       \tab x      \tab x           \tab x      \tab x      \tab x    \cr
#'   font       \tab       \tab       \tab x      \tab x           \tab x      \tab x      \tab x    \cr
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
#' @param patterns a logical, vector, or list of graphical parameters for
#'   fill patterns in the diagram. Vectors are assumed to be pattern types
#'   (currently `"stripes"` or `NA`), where `NA` means no pattern.
#'   Supported list items are `type`, `angle`, `col`, `lwd`, and `alpha`.
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
#'   default are the original values in the input to [euler()]. In addition
#'   to plain vectors, `quantities$labels` can also be a named vector keyed by
#'   subset names (e.g., `"A"`, `"B"`, `"A&B"`), which is useful for supplying
#'   custom text for overlap regions. If `quantities$labels` is `NULL`,
#'   `quantities$format` can be used to control number formatting as a list with
#'   an item `fun` (a function such as [signif()] or [round()]) and optional
#'   extra arguments passed to that function. `quantities$total` can be used to
#'   set an external
#'   denominator for percent/fraction quantities (instead of the plotted total).
#'   to arguments that apply to [grid::grid.text()], an argument `type` may
#'   also be used which should be a combination of `"counts"`, `"percent"`,
#'   and `"fraction"`. The first item will be printed first and the second
#'   will be printed thereafter inside brackets. The default is
#'   `type = "counts"`.
#' @param strips a list, ignored unless the `'by'` argument
#'   was used in [euler()]
#' @param bg a logical, character, or list controlling the background grob.
#'   Character values are interpreted as the background fill color.
#' @param n number of vertices for the `edges` and `fills`
#' @param main a title for the plot in the form of a
#'   character, expression, list or something that can be
#'   sensibly converted to a label via [grDevices::as.graphicsAnnot()]. A
#'   list of length one can be provided, in which case its only element
#'   is used as the label. If a list of longer length is provided, an item
#'   named `'label'` must be provided (and will be used for the actual text).
#' @param ... parameters to update `fills` and `edges` with and thereby a shortcut
#'   to set these parameters
#'   [grid::grid.text()].
#' @param adjust_labels a logical. If `TRUE`, adjustment will be made to avoid
#'   overlaps or out-of-limits plotting of labels, quantities, and
#'   percentages.
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
#' plot(fit, fills = "transparent", lty = 1:2)
#'
#' # Save plot parameters to plot using some other method
#' diagram_description <- plot(fit)
#'
#' # Plots using 'by' argument
#' plot(euler(fruits[, 1:4], by = list(sex)), legend = TRUE)
plot.euler <- function(
  x,
  fills = TRUE,
  patterns = FALSE,
  edges = TRUE,
  legend = FALSE,
  labels = identical(legend, FALSE),
  quantities = FALSE,
  strips = NULL,
  bg = FALSE,
  main = NULL,
  n = 200L,
  adjust_labels = TRUE,
  ...
) {
  if (!missing(adjust_labels)) {
    warning("`adjust_labels` is deprecated and no longer has any effect.")
  }

  # retrieve default options
  opar <- eulerr_options()

  groups <- attr(x, "groups")
  fills_user <- fills
  dots <- list(...)

  do_custom_legend <- grid::is.grob(legend)

  do_fills <- !is_false(fills) && !is.null(fills)
  do_patterns <- !is_false(patterns) && !is.null(patterns)
  do_edges <- !is_false(edges) && !is.null(edges)
  do_labels <- !is_false(labels) && !is.null(labels)
  do_quantities <- !is_false(quantities) && !is.null(quantities)
  do_legend <- !is_false(legend) && !is.null(legend)
  do_groups <- !is.null(groups)
  do_strips <- !is_false(strips) && do_groups
  do_bg <- !is_false(bg) && !is.null(bg)
  do_main <- is.character(main) || is.expression(main) || is.list(main)

  ellipses <- if (do_groups) x[[1L]]$ellipses else x$ellipses

  n_e <- NROW(ellipses)
  n_id <- 2^n_e - 1
  id <- bit_indexr(n_e)

  setnames <- rownames(ellipses)

  if (do_groups) {
    res <- lapply(x, function(xi) is.na(xi$ellipses)[, 1L])
    empty_sets <- apply(do.call(rbind, res), 2, all)

    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

    res <- lapply(x, function(xi) {
      fitted <- xi$fitted.values[!empty_subsets]
      nonzero <- nonzero_fit(fitted)
      ifelse(is.na(nonzero), FALSE, nonzero)
    })

    nonzero <- apply(do.call(rbind, res), 2, any)
  } else {
    empty_sets <- is.na(x$ellipses[, 1L])
    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0
    fitted <- x$fitted.values[!empty_subsets]
    nonzero <- nonzero_fit(fitted)
    nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)
  }

  merged_sets <- rep(FALSE, length(setnames))

  if (!do_groups && any(nonzero)) {
    n_overlaps <- integer(n_id)
    single_mass <- logical(n_e)

    for (i in seq_len(n_e)) {
      nz <- nonzero_fit(x$fitted.values)
      nzi <- nz & id[, i]

      if (sum(nzi) == 1) {
        n_overlaps[which(nzi)] <- n_overlaps[which(nzi)] + 1
      }

      single_mass[i] <- sum(nzi) == 1
    }

    complete_overlaps <- n_overlaps > 1

    merge_sets <- id[complete_overlaps, ] & single_mass

    if (any(merge_sets)) {
      setnames[merge_sets] <- paste(setnames[merge_sets], collapse = ",")
      merged_sets[which(merge_sets)[length(which(merge_sets))]] <- TRUE
    }
  }

  stopifnot(n > 0, is.numeric(n) && length(n) == 1)

  fills_out <- NULL

  # setup fills
  if (do_fills) {
    fills_out <- replace_list(
      list(
        fill = opar$fills$fill,
        alpha = opar$fills$alpha
      ),
      if (is.list(fills)) {
        fills
      } else if (isTRUE(fills)) {
        list()
      } else {
        list(fill = fills)
      }
    )
    fills_out <- replace_list(fills_out, dots)
    fills_out$col <- "transparent"

    if (is.function(fills_out$fill)) {
      fills_out$fill <- fills_out$fill(n_e)
    }

    n_fills <- length(fills_out$fill)
    if (n_fills == n_e && n_fills < n_id) {
      for (i in (n_fills + 1L):n_id) {
        fills_out$fill[i] <- mix_colors(fills_out$fill[which(id[i, ])])
      }
    } else if (!(n_fills %in% c(1L, n_id))) {
      stop("`fills$fill` must have length 1, n_sets, or n_subsets.")
    }
    fills <- list()
    fills$gp <- setup_gpar(fills_out, list(), n_id)
  } else {
    fills <- NULL
  }

  legacy_patterns <- list(
    type = if (is.list(fills_user) && !is.null(fills_user$pattern)) {
      fills_user$pattern
    } else {
      NULL
    },
    angle = if (is.list(fills_user) && !is.null(fills_user$angle)) {
      fills_user$angle
    } else {
      NULL
    },
    col = if (is.list(fills_user) && !is.null(fills_user$pattern_col)) {
      fills_user$pattern_col
    } else {
      NULL
    },
    lwd = if (is.list(fills_user) && !is.null(fills_user$pattern_lwd)) {
      fills_user$pattern_lwd
    } else {
      NULL
    }
  )
  do_legacy_patterns <- any(!vapply(legacy_patterns, is.null, logical(1)))
  do_patterns <- do_patterns || do_legacy_patterns

  # setup patterns
  if (do_patterns && !do_fills) {
    stop("`patterns` requires `fills` to be enabled.")
  }

  if (do_patterns) {
    patterns_out <- replace_list(
      list(
        type = opar$patterns$type,
        angle = opar$patterns$angle,
        col = opar$patterns$col,
        lwd = opar$patterns$lwd,
        alpha = opar$patterns$alpha
      ),
      if (is.list(patterns)) {
        patterns
      } else if (isTRUE(patterns)) {
        list(type = "stripes")
      } else {
        list(type = patterns)
      }
    )

    if (do_legacy_patterns) {
      warning(
        "`fills$pattern`, `fills$angle`, `fills$pattern_col`, and ",
        "`fills$pattern_lwd` are deprecated; use `patterns` instead."
      )
      if (!is.null(legacy_patterns$type)) {
        patterns_out$type <- legacy_patterns$type
      }
      if (!is.null(legacy_patterns$angle)) {
        patterns_out$angle <- legacy_patterns$angle
      }
      if (!is.null(legacy_patterns$col)) {
        patterns_out$col <- legacy_patterns$col
      }
      if (!is.null(legacy_patterns$lwd)) patterns_out$lwd <- legacy_patterns$lwd
    }

    patterns_out <- replace_list(patterns_out, dots)
    expand_pattern_param <- function(x, name, by_set, default = NULL) {
      n_x <- length(x)
      if (!(n_x %in% c(1L, n_e, n_id))) {
        stop("`patterns$", name, "` must have length 1, n_sets, or n_subsets.")
      }
      if (n_x == 1L || n_x == n_id) {
        return(rep_len(x, n_id))
      }

      out <- vector(mode = mode(x), length = n_id)
      for (ii in seq_len(n_id)) {
        active <- which(id[ii, ] & by_set)
        if (length(active) == 0L) {
          if (is.null(default)) {
            out[ii] <- x[1L]
          } else if (is.character(default)) {
            out[ii] <- default[1L]
          } else {
            out[ii] <- as.vector(default)[1L]
          }
        } else if (is.character(x)) {
          out[ii] <- if (length(active) == 1L) {
            x[active]
          } else {
            mix_colors(x[active])
          }
        } else {
          out[ii] <- mean(x[active])
        }
      }
      out
    }

    n_types <- length(patterns_out$type)
    if (!(n_types %in% c(1L, n_e, n_id))) {
      stop("`patterns$type` must have length 1, n_sets, or n_subsets.")
    }

    if (n_types %in% c(1L, n_id)) {
      pattern_mode <- "intersection"
      patterns_out$type <- rep_len(patterns_out$type, n_id)
      patterns_out$angle <- expand_pattern_param(patterns_out$angle, "angle", rep(TRUE, n_e))
      patterns_out$col <- expand_pattern_param(patterns_out$col, "col", rep(TRUE, n_e), default = NA_character_)
      patterns_out$lwd <- expand_pattern_param(patterns_out$lwd, "lwd", rep(TRUE, n_e))
      patterns_out$alpha <- expand_pattern_param(patterns_out$alpha, "alpha", rep(TRUE, n_e))
    } else {
      pattern_mode <- "shape"
      patterns_out$type <- tolower(patterns_out$type)
      patterns_out$angle <- rep_len(patterns_out$angle, n_e)
      patterns_out$col <- rep_len(patterns_out$col, n_e)
      patterns_out$lwd <- rep_len(patterns_out$lwd, n_e)
      patterns_out$alpha <- rep_len(patterns_out$alpha, n_e)
    }

    patterns <- list()
    patterns$mode <- pattern_mode

    if (pattern_mode == "intersection") {
      patterns_out$type[is.na(patterns_out$type)] <- "none"
      patterns_out$type <- tolower(patterns_out$type)

      if (any(!patterns_out$type %in% c("none", "stripes"))) {
        stop("`patterns$type` must be one of: 'stripes', NA.")
      }

      if (all(is.na(patterns_out$col))) {
        patterns_out$col <- fills$gp$fill
      } else {
        na_col <- is.na(patterns_out$col) | patterns_out$col == "NA"
        patterns_out$col[na_col] <- fills$gp$fill[na_col]
      }

      patterns$gp <- setup_gpar(patterns_out, list(), n_id)
    } else {
      patterns_out$type[is.na(patterns_out$type)] <- "none"
      if (any(!patterns_out$type %in% c("none", "stripes"))) {
        stop("`patterns$type` must be one of: 'stripes', NA.")
      }
      if (all(is.na(patterns_out$col))) {
        patterns_out$col <- fills$gp$fill[seq_len(n_e)]
      } else {
        na_col <- is.na(patterns_out$col) | patterns_out$col == "NA"
        patterns_out$col[na_col] <- fills$gp$fill[which(na_col)]
      }
      patterns$set_gp <- setup_gpar(patterns_out, list(), n_e)
      patterns$gp <- patterns$set_gp
    }
  } else {
    patterns <- NULL
  }

  # setup edges
  if (do_edges) {
    if (isTRUE(edges)) {
      edges_out <- list()
    } else if (!is.list(edges)) {
      edges_out <- list(col = edges)
    } else {
      edges_out <- edges
    }

    edges <- list()
    edges$gp <- setup_gpar(
      list(
        col = opar$edges$col,
        alpha = opar$edges$alpha,
        lex = opar$edges$lex,
        lwd = opar$edges$lwd,
        lty = opar$edges$lty
      ),
      update_list(edges_out, dots),
      n_e
    )
  } else {
    edges <- NULL
  }

  # setup strips
  if (do_groups) {
    group_names <- lapply(groups, levels)
    n_levels <- sum(lengths(group_names))
  }

  if (do_strips) {
    strips <- list(
      gp = setup_gpar(opar$strips, strips, n_levels),
      groups = groups
    )
  } else {
    strips <- NULL
  }

  # setup labels
  if (do_labels) {
    if (is.list(labels)) {
      labels <- update_list(
        list(labels = setnames, rot = opar$labels$rot),
        labels
      )
    } else if (isTRUE(labels)) {
      labels <- list(labels = setnames, rot = opar$labels$rot)
    } else {
      labels <- list(labels = labels, rot = opar$labels$rot)
    }

    labels$rot <- rep_len(labels$rot, n_e)
    labels$gp <- setup_gpar(
      list(
        col = opar$labels$col,
        alpha = opar$labels$alpha,
        fontsize = opar$labels$fontsize,
        cex = opar$labels$cex,
        fontfamily = opar$labels$fontfamily,
        lineheight = opar$labels$lineheight,
        font = opar$labels$font
      ),
      labels,
      n_e
    )
  } else {
    labels <- NULL
  }

  # setup quantities
  if (do_quantities) {
    normalize_quantity_formatter <- function(formatter) {
      if (is.null(formatter)) {
        formatter <- list()
      }

      if (!is.list(formatter)) {
        stop("`quantities$format` must be a list.")
      }

      if (is.null(formatter$fun)) {
        fun <- NULL
      } else {
        fun <- formatter$fun
      }

      if (!is.null(fun) && !is.function(fun)) {
        stop("`quantities$format$fun` must be a function.")
      }

      args <- formatter[setdiff(names(formatter), "fun")]
      list(fun = fun, args = args)
    }

    if (is.list(quantities)) {
      if (!is.null(quantities$type)) {
        quantities$type[quantities$type == "numbers"] <- "counts"
        if (!all(quantities$type %in% c("counts", "percent", "fraction"))) {
          stop(
            "'type' must be one or more of 'counts', 'percent', and 'fraction'"
          )
        }

        quantities_type <- match.arg(
          quantities$type,
          c("counts", "percent", "fraction"),
          several.ok = TRUE
        )
      } else {
        quantities_type <- opar$quantities$type
      }

      quantities <- update_list(
        list(
          labels = NULL,
          type = quantities_type,
          rot = opar$quantities$rot,
          format = NULL,
          total = NULL
        ),
        quantities
      )
    } else if (isTRUE(quantities)) {
      quantities <- list(
        labels = NULL,
        type = opar$quantities$type,
        rot = opar$quantities$rot,
        format = NULL,
        total = NULL
      )
    } else {
      quantities <- list(
        labels = quantities,
        type = opar$quantities$type,
        rot = opar$quantities$rot,
        format = NULL,
        total = NULL
      )
    }

    quantities$type[quantities$type == "numbers"] <- "counts"
    quantities$format <- normalize_quantity_formatter(quantities$format)

    if (
      !is.null(quantities$total) &&
        (!is.numeric(quantities$total) ||
          length(quantities$total) != 1 ||
          is.na(quantities$total) ||
          quantities$total <= 0)
    ) {
      stop("`quantities$total` must be a single positive number.")
    }

    quantities$rot <- rep_len(quantities$rot, n_id)

    quantities$gp <- setup_gpar(
      list(
        col = opar$quantities$col,
        alpha = opar$quantities$alpha,
        fontsize = opar$quantities$fontsize,
        cex = opar$quantities$cex,
        fontfamily = opar$quantities$fontfamily,
        lineheight = opar$quantities$lineheight,
        font = opar$quantities$font
      ),
      quantities,
      n_id
    )
  } else {
    quantities <- NULL
  }

  # setup legend
  if (do_custom_legend) {
    legend <- legend
  } else if (do_legend) {
    # TODO: create a better, custom legend

    legend <- update_list(
      list(
        labels = setnames[!empty_sets & !merged_sets],
        side = opar$legend$side,
        nrow = sum(!empty_sets),
        ncol = 1L,
        byrow = opar$legend$byrow,
        do.lines = opar$legend$do.lines,
        lines.first = opar$legend$lines.first,
        hgap = opar$legend$hgap,
        vgap = opar$legend$vgap,
        default.units = opar$legend$default.units,
        pch = opar$legend$pch
      ),
      legend
    )

    legend$gp <- setup_gpar(
      list(
        fill = if (do_fills) {
          fills$gp$fill[!empty_sets & !merged_sets]
        } else {
          "transparent"
        },
        alpha = if (do_fills) {
          fills$gp$alpha[!empty_sets & !merged_sets]
        } else if (do_edges) {
          edges$gp$alpha[!empty_sets & !merged_sets]
        } else {
          0
        },
        cex = opar$legend$cex,
        fontsize = opar$legend$fontsize / opar$legend$cex,
        font = opar$legend$font,
        fontfamily = opar$legend$fontfamily,
        lwd = if (do_edges) edges$gp$lwd[!empty_sets & !merged_sets] else 0,
        lex = if (do_edges) edges$gp$lex[!empty_sets & !merged_sets] else 0,
        col = if (do_edges) {
          edges$gp$col[!empty_sets & !merged_sets]
        } else {
          "transparent"
        },
        pattern_type = if (do_patterns) {
          patterns$gp$type[!empty_sets & !merged_sets]
        } else {
          "none"
        },
        pattern_angle = if (do_patterns) {
          patterns$gp$angle[!empty_sets & !merged_sets]
        } else {
          45
        },
        pattern_col = if (do_patterns) {
          patterns$gp$col[!empty_sets & !merged_sets]
        } else {
          "transparent"
        },
        pattern_lwd = if (do_patterns) {
          patterns$gp$lwd[!empty_sets & !merged_sets]
        } else {
          0
        },
        pattern_alpha = if (do_patterns) {
          patterns$gp$alpha[!empty_sets & !merged_sets]
        } else {
          0
        }
      ),
      legend,
      sum(!empty_sets)
    )
  } else {
    legend <- NULL
  }

  if (do_main) {
    if (is.list(main)) {
      if (length(main) == 1 && is.null(names(main))) {
        label <- main[[1]]
      } else {
        label <- main$label
      }

      if (is.null(label)) {
        stop("you need to provide a 'label' item if 'main' is a list.")
      }
    } else {
      label <- main
    }

    main <- update_list(
      list(
        label = label,
        x = opar$main$x,
        y = opar$main$y,
        just = opar$main$just,
        hjust = opar$main$hjust,
        vjust = opar$main$vjust,
        rot = opar$main$rot,
        check.overlap = opar$main$check.overlap,
        default.units = opar$main$default.units
      ),
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

  if (do_bg) {
    if (isTRUE(bg)) {
      bg <- list()
    } else if (!is.list(bg)) {
      bg <- list(fill = bg)
    }

    bg_grob <- grid::rectGrob(
      gp = setup_gpar(
        list(fill = "white", col = "transparent", alpha = 1),
        bg,
        1
      ),
      name = "bg.grob"
    )
  }

  # set up geometry for diagrams
  if (do_groups) {
    data <- lapply(
      x,
      setup_geometry,
      fills = fills,
      edges = edges,
      labels = labels,
      quantities = quantities,
      n = n,
      id = id,
      merged_sets = merged_sets
    )
  } else {
    data <- setup_geometry(
      x,
      fills = fills,
      edges = edges,
      labels = labels,
      quantities = quantities,
      n = n,
      id = id,
      merged_sets = merged_sets
    )
  }

  # start setting up grobs

  if (do_groups) {
    n_groups <- length(data)
    euler_grob_children <- grid::gList()
    for (i in seq_len(n_groups)) {
      euler_grob_children[[i]] <- setup_grobs(
        data[[i]],
        fills = fills,
        patterns = patterns,
        edges = edges,
        labels = labels,
        quantities = quantities,
        number = i,
        merged_sets = merged_sets
      )
    }
    euler_grob <- grid::gTree(
      grid::nullGrob(),
      name = "canvas.grob",
      children = euler_grob_children
    )
    pos <- vapply(groups, as.numeric, numeric(NROW(groups)), USE.NAMES = FALSE)
    layout <- lengths(lapply(groups, unique))
    if (length(layout) == 1L) {
      layout <- c(1L, layout)
    }
    xlim <- range(unlist(lapply(data, "[[", "xlim")))
    ylim <- range(unlist(lapply(data, "[[", "ylim")))
  } else {
    euler_grob <- setup_grobs(
      data,
      fills = fills,
      patterns = patterns,
      edges = edges,
      labels = labels,
      quantities = quantities,
      number = 1,
      merged_sets = merged_sets
    )
    euler_grob <- grid::grobTree(euler_grob, name = "canvas.grob")

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

  ar <- xrng / yrng
  # adjust <- layout[1L]/layout[2]

  do_strip_left <- layout[1L] > 1L && do_strips
  do_strip_top <- layout[2L] > 1L && do_strips

  strip_top_row <- strip_top_col <- strip_left_row <- strip_left_col <- 1

  nrow <- ncol <- 1
  heights <- grid::unit(1, "null")
  widths <- grid::unit(1 * ar * layout[2] / layout[1], "null")
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
      grid::viewport(
        layout.pos.row = strip_top_row,
        layout.pos.col = strip_top_col,
        name = "strip.top.vp",
        layout = grid::grid.layout(nrow = 1, ncol = layout[2])
      )

    lvls <- levels(strips$groups[[names(layout)[[2]]]])
    n_lvls <- length(lvls)
    step <- 1 / n_lvls

    strip_top_grob <- grid::textGrob(
      lvls,
      x = step / 2 + (seq(0, n_lvls - 1) * step),
      name = "strip.top.grob",
      gp = do.call(grid::gpar, strips$gp),
      vp = strip_top_vp
    )

    heights <- grid::unit.c(
      grid::unit(2, "grobheight", list(strip_top_grob)),
      heights
    )
  }

  if (do_strip_left) {
    strip_left_vp <-
      grid::viewport(
        layout.pos.row = strip_left_row,
        layout.pos.col = strip_left_col,
        name = "strip.left.vp",
        layout = grid::grid.layout(nrow = layout[1], ncol = 1)
      )

    lvls <- rev(levels(strips$groups[[names(layout)[[1]]]]))
    n_lvls <- length(lvls)
    step <- 1 / n_lvls

    strip_left_grob <- grid::textGrob(
      lvls,
      y = step / 2 + (seq(0, n_lvls - 1) * step),
      name = "strip.left.grob",
      rot = 90,
      gp = do.call(grid::gpar, strips$gp),
      vp = strip_left_vp
    )

    widths <- grid::unit.c(
      grid::unit(2, "grobwidth", list(strip_left_grob)),
      widths
    )
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
      if (do_patterns) {
        legend_grob <- add_legend_patterns(legend_grob, legend$gp)
      }
    }

    legend_grob$name <- "legend.grob"
    if (do_strip_top) {
      legend_row <- 2 + 2 * do_main
    }

    if (legend$side == "right") {
      # legend on right (default)
      ncol <- ncol + 2L
      legend_row <- nrow
      legend_col <- ncol
      widths <- grid::unit.c(
        widths,
        grid::unit(c(1, 1), c("lines", "grobwidth"), list(NULL, legend_grob))
      )
    } else if (legend$side == "left") {
      # legend on left
      ncol <- ncol + 2L
      legend_row <- if (do_strip_top) 2L + 2 * do_main else 1L + 2 * do_main
      legend_col <- 1
      diagram_col <- diagram_col + 2L
      if (do_strip_left) {
        strip_left_col <- strip_left_col + 2L
      }
      if (do_strip_top) {
        strip_top_col <- strip_top_col + 2L
      }
      widths <- grid::unit.c(
        grid::unit(c(1, 1), c("grobwidth", "lines"), list(legend_grob, NULL)),
        widths
      )
    } else if (legend$side == "top") {
      # legend on top
      nrow <- nrow + 2L
      legend_row <- 1L + do_main * 2
      legend_col <- if (do_strip_left) 2L else 1L
      diagram_row <- diagram_row + 2L
      if (do_strip_top) {
        strip_top_row <- strip_top_row + 2L
      }
      if (do_strip_left) {
        strip_left_row <- strip_left_row + 2L
      }
      heights <- grid::unit.c(
        grid::unit(c(1, 1), c("grobheight", "lines"), list(legend_grob, NULL)),
        heights
      )
    } else {
      # legend on bottom
      nrow <- nrow + 2L
      legend_row <- nrow
      legend_col <- if (do_strip_left) 2L else 1L
      heights <- grid::unit.c(
        heights,
        grid::unit(c(1, 1), c("lines", "grobheight"), list(NULL, legend_grob))
      )
    }
    legend_grob$vp <- grid::viewport(
      layout.pos.row = legend_row,
      layout.pos.col = legend_col,
      name = "legend.vp"
    )
  }

  if (do_main) {
    main_grob <- grid::textGrob(
      label = main$label,
      x = main$x,
      y = main$y,
      just = main$just,
      hjust = main$hjust,
      vjust = main$vjust,
      rot = main$rot,
      check.overlap = FALSE,
      default.units = main$default.units,
      gp = main$gp,
      name = "main.grob"
    )
    heights <- grid::unit.c(
      grid::unit(c(1, 1), c("lines", "grobheight"), list(NULL, main_grob)),
      heights
    )
    main_grob$vp <- grid::viewport(
      layout.pos.row = 1,
      layout.pos.col = diagram_col,
      name = "main.vp"
    )
  }

  canvas_vp <- grid::viewport(
    layout.pos.row = diagram_row,
    layout.pos.col = diagram_col,
    name = "canvas.vp",
    layout = grid::grid.layout(
      nrow = layout[1L],
      ncol = layout[2L],
      widths = rep(1 / ar, layout[1L]),
      heights = rep(1, layout[2L])
    )
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
  children <- gList(
    if (do_bg) bg_grob = bg_grob,
    if (do_main) main_grob = main_grob,
    if (do_strip_top) strip_top_grob = strip_top_grob,
    if (do_strip_left) strip_left_grob = strip_left_grob,
    if (do_legend) legend_grob = legend_grob,
    euler_grob = euler_grob
  )

  grid::gTree(
    data = data,
    children = children,
    vp = grid::viewport(
      layout = grid::grid.layout(
        nrow = nrow,
        ncol = ncol,
        widths = widths,
        heights = heights,
        respect = TRUE
      ),
      name = "euler.vp"
    ),
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

add_legend_patterns <- function(legend_grob, gp) {
  point_cells <- which(vapply(
    legend_grob$children,
    function(cell) {
      inherits(cell$children[[1]], "points") ||
        inherits(cell$children[[1]], "gTree")
    },
    logical(1)
  ))

  if (length(point_cells) == 0L) {
    return(legend_grob)
  }

  for (i in seq_along(point_cells)) {
    t <- seq(0, 2 * pi, length.out = 64)
    circle <- list(
      x = 0.5 + 0.48 * cos(t),
      y = 0.5 + 0.48 * sin(t)
    )

    cell <- legend_grob$children[[point_cells[i]]]
    point_vp <- cell$children[[1]]$vp

    base_symbol <- grid::pathGrob(
      x = circle$x,
      y = circle$y,
      default.units = "npc",
      gp = grid::gpar(
        fill = gp$fill[i],
        col = gp$col[i],
        lwd = gp$lwd[i],
        lex = gp$lex[i],
        alpha = gp$alpha[i]
      )
    )

    symbol_children <- grid::gList(base_symbol)

    if (gp$pattern_type[i] == "stripes") {
      pcol <- gp$pattern_col[i]
      if (is.na(pcol)) {
        pcol <- gp$fill[i]
      }

      clipped <- apply_stripe_pattern(
        fill_data = list(
          x = circle$x,
          y = circle$y,
          id.lengths = length(circle$x)
        ),
        pattern_gp = list(
          type = "stripes",
          angle = gp$pattern_angle[i],
          col = pcol,
          lwd = gp$pattern_lwd[i],
          alpha = gp$pattern_alpha[i]
        ),
        spacing_scale = 3
      )

      if (!is.null(clipped)) {
        stripe_grob <- grid::pathGrob(
          x = unlist(lapply(clipped, "[[", "x"), use.names = FALSE),
          y = unlist(lapply(clipped, "[[", "y"), use.names = FALSE),
          id.lengths = lengths(lapply(clipped, "[[", "x")),
          default.units = "npc",
          gp = grid::gpar(
            fill = pcol,
            col = "transparent",
            alpha = gp$pattern_alpha[i]
          )
        )
        symbol_children <- grid::gList(base_symbol, stripe_grob)
      }
    }

    cell$children[[1]] <- grid::grobTree(
      children = symbol_children,
      vp = point_vp
    )
    legend_grob$children[[point_cells[i]]] <- cell
  }

  legend_grob
}

#' @rdname plot.euler
#' @export
plot.eulerr_venn <- function(
  x,
  fills = TRUE,
  patterns = FALSE,
  edges = TRUE,
  legend = FALSE,
  labels = identical(legend, FALSE),
  quantities = TRUE,
  strips = NULL,
  bg = FALSE,
  main = NULL,
  n = 200L,
  adjust_labels = TRUE,
  ...
) {
  if (!missing(adjust_labels)) {
    warning("`adjust_labels` is deprecated and no longer has any effect.")
  }

  plot.euler(
    x = x,
    fills = fills,
    patterns = patterns,
    edges = edges,
    legend = legend,
    labels = labels,
    quantities = quantities,
    strips = strips,
    bg = bg,
    main = main,
    n = n,
    ...
  )
}

#' @rdname plot.euler
#' @export
plot.venn <- function(...) {
  warning(
    "`plot.venn()` is deprecated and will be removed in a future release. ",
    "Use `plot()` on objects from `venn()` instead."
  )
  plot.eulerr_venn(...)
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
  if (isTRUE(newpage)) {
    grid::grid.newpage()
  }
  grid::grid.draw(x)
}

#' @rdname plot.eulergram
#' @export
print.eulergram <- function(x, ...) {
  graphics::plot(x, ...)
}
