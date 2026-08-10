# Interactive (htmlwidgets) rendering backend for eulerr. This mirrors the
# static `plot.euler()` pipeline: it reuses the shared geometry engine
# (`setup_geometry()`) and the shared fill resolver (`resolve_region_fills()`)
# so an interactive diagram looks identical to its static counterpart, then
# serializes the device-independent geometry to a plain list for the JS
# renderer in `inst/htmlwidgets/eulerr.js`.

#' Interactive Euler and Venn diagrams
#'
#' Render a fitted diagram as an interactive HTML widget. Hovering a region
#' highlights it and shows a tooltip with the region's set combination and
#' quantity. Unlike [plot.euler()], which draws static `grid` graphics, this
#' backend produces an [htmlwidgets::htmlwidget] suitable for HTML reports,
#' Shiny apps, and the RStudio Viewer.
#'
#' The interactive backend depends on \pkg{htmlwidgets} and \pkg{jsonlite},
#' which are listed under `Suggests`. Install them with
#' `install.packages(c("htmlwidgets", "jsonlite"))`.
#'
#' @param x an object of class `euler` or `venn`, from [euler()] or [venn()].
#' @param fills a logical, vector of colors, or list of graphical parameters
#'   for the region fills, following the same rules as the `fills` argument of
#'   [plot.euler()].
#' @param edges a logical or list controlling the set outlines.
#' @param labels a logical or list controlling the set labels.
#' @param quantities a logical or list controlling the displayed quantities.
#'   Defaults to `TRUE` for `venn` objects and `FALSE` otherwise, matching
#'   [plot.euler()].
#' @param complement a logical controlling whether the container box and
#'   complement count are drawn (only relevant for diagrams fit with
#'   `complement =`).
#' @param n number of vertices used to render each shape.
#' @param width,height widget dimensions passed to
#'   [htmlwidgets::createWidget()]. Usually left `NULL` so the widget fills its
#'   container.
#' @param elementId an optional string id for the widget root element.
#' @param ... additional named graphical parameters forwarded to the fill and
#'   edge styling (a shortcut, as in [plot.euler()]).
#'
#' @return An object of class `htmlwidget` that renders as an interactive SVG
#'   Euler/Venn diagram.
#'
#' @seealso [euler()], [venn()], [plot.euler()]
#'
#' @examples
#' if (requireNamespace("htmlwidgets", quietly = TRUE)) {
#'   fit <- euler(c(A = 10, B = 5, "A&B" = 3))
#'   euler_widget(fit)
#'
#'   # venn objects show quantities by default
#'   euler_widget(venn(c(A = 10, B = 5, "A&B" = 3)))
#' }
#'
#' @export
euler_widget <- function(x, ...) {
  UseMethod("euler_widget")
}

#' @describeIn euler_widget Render an `euler` or (by inheritance) `venn` object.
#' @export
euler_widget.euler <- function(
  x,
  fills = TRUE,
  edges = TRUE,
  labels = TRUE,
  quantities = inherits(x, "venn"),
  complement = TRUE,
  n = 200L,
  width = NULL,
  height = NULL,
  elementId = NULL,
  ...
) {
  check_suggests(c("htmlwidgets", "jsonlite"))

  if (!is.null(attr(x, "groups"))) {
    stop(
      "`euler_widget()` does not support faceted (`by =`) diagrams yet.",
      call. = FALSE
    )
  }

  opar <- eulerr_options()
  dots <- list(...)

  do_fills <- !is_false(fills) && !is.null(fills)
  do_edges <- !is_false(edges) && !is.null(edges)
  do_labels <- !is_false(labels) && !is.null(labels)
  do_quantities <- !is_false(quantities) && !is.null(quantities)
  do_complement <- !is.null(x$container) &&
    !is_false(complement) &&
    !is.null(complement)

  stopifnot(is.numeric(n), length(n) == 1L, n > 0)

  idx <- build_region_index(x)
  n_e <- idx$n_e
  n_id <- idx$n_id
  setnames <- idx$setnames

  # --- geometry parameters -------------------------------------------------
  # setup_geometry() only needs presence flags plus label names, quantity
  # formatting, and font gpars (for label-size measurement during placement).
  labels_param <- if (do_labels) {
    lp <- list(labels = setnames, rot = rep_len(opar$labels$rot, n_e))
    lp$gp <- setup_gpar(
      list(
        col = opar$labels$col,
        alpha = opar$labels$alpha,
        fontsize = opar$labels$fontsize,
        cex = opar$labels$cex,
        fontfamily = opar$labels$fontfamily,
        lineheight = opar$labels$lineheight,
        font = opar$labels$font
      ),
      list(),
      n_e
    )
    lp
  } else {
    NULL
  }

  quantities_param <- if (do_quantities) {
    qp <- list(
      labels = if (is.list(quantities) || isTRUE(quantities)) NULL else quantities,
      type = if (is.list(quantities) && !is.null(quantities$type)) {
        quantities$type
      } else {
        opar$quantities$type
      },
      template = if (is.list(quantities) && !is.null(quantities$template)) {
        quantities$template
      } else {
        opar$quantities$template
      },
      rot = rep_len(opar$quantities$rot, n_id),
      format = list(fun = NULL, args = list()),
      total = if (is.list(quantities)) quantities$total else NULL
    )
    qp$gp <- setup_gpar(
      list(
        col = opar$quantities$col,
        alpha = opar$quantities$alpha,
        fontsize = opar$quantities$fontsize,
        cex = opar$quantities$cex,
        fontfamily = opar$quantities$fontfamily,
        lineheight = opar$quantities$lineheight,
        font = opar$quantities$font
      ),
      list(),
      n_id
    )
    qp
  } else {
    NULL
  }

  placement_opts <- if (do_labels || do_quantities || do_complement) {
    strat <- opar$labels$placement
    list(
      placement = strat,
      margin = opar$labels$margin,
      iterations = if (identical(strat, "force_directed")) {
        (opar$labels$force_directed %||% list())$iterations
      } else {
        NULL
      },
      min_gap = if (identical(strat, "elbow")) {
        (opar$labels$elbow %||% list())$min_gap
      } else {
        NULL
      },
      tether = opar$labels$tether,
      gap = opar$labels$gap
    )
  } else {
    NULL
  }

  geom <- setup_geometry(
    x,
    fills = list(), # always compute region polygons (hover targets)
    edges = if (do_edges) list() else NULL,
    labels = labels_param,
    quantities = quantities_param,
    annotations = NULL,
    n = as.integer(n),
    merged_sets = rep(FALSE, n_e),
    placement_opts = placement_opts,
    do_complement_label = isTRUE(do_complement)
  )

  # --- resolved fill colors (shared with plot.euler) -----------------------
  rf <- if (do_fills) {
    resolve_region_fills(fills, dots, idx$id, setnames, n_e, n_id, opar)
  } else {
    NULL
  }

  payload <- build_widget_payload(
    geom,
    rf,
    idx,
    opar,
    do_fills = do_fills,
    do_edges = do_edges,
    do_complement = do_complement
  )

  htmlwidgets::createWidget(
    name = "eulerr",
    x = payload,
    width = width,
    height = height,
    package = "eulerr",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = 400,
      padding = 0,
      browser.fill = TRUE,
      viewer.fill = TRUE
    )
  )
}

#' Assemble the plain-data payload serialized to the JS renderer
#'
#' Converts the geometry from [setup_geometry()] and the resolved fills from
#' [resolve_region_fills()] into a list of atomics (no data frames or factors),
#' aligning geometry regions to fill parameters by combination label.
#'
#' @keywords internal
#' @noRd
build_widget_payload <- function(
  geom,
  rf,
  idx,
  opar,
  do_fills,
  do_edges,
  do_complement
) {
  geom_labels <- names(geom$fitted.values)
  n_geo <- length(geom_labels)

  # Align geometry order (names(x$fitted.values)) to parameter order
  # (build_region_index combo order) by label, mirroring setup_grobs().
  par_idx <- match(geom_labels, idx$combo_labels)

  region_fill <- if (do_fills) to_hex(rf$gp$fill)[par_idx] else rep(NA_character_, n_geo)
  region_alpha <- if (do_fills) rf$gp$alpha[par_idx] else rep(0, n_geo)

  regions <- list()
  for (j in seq_len(n_geo)) {
    fg <- geom$fills[[j]]
    if (is.null(fg) || length(fg$id.lengths) == 0L) {
      next
    }
    lbl <- geom_labels[j]
    count <- geom$original.values[[lbl]]
    regions[[length(regions) + 1L]] <- list(
      label = lbl,
      quantity = if (is.finite(count)) format_quantity(count) else NULL,
      fill = if (is.na(region_fill[j])) NULL else region_fill[j],
      alpha = region_alpha[j],
      x = as.numeric(fg$x),
      y = as.numeric(fg$y),
      id_lengths = as.integer(fg$id.lengths)
    )
  }

  edges <- list()
  if (do_edges && length(geom$set_polygons) > 0L) {
    set_names_geo <- rownames(geom$shapes)
    ecol <- to_hex(opar$edges$col)
    for (i in seq_along(geom$set_polygons)) {
      sp <- geom$set_polygons[[i]]
      edges[[length(edges) + 1L]] <- list(
        set = set_names_geo[i],
        col = ecol,
        lwd = opar$edges$lwd,
        x = as.numeric(sp$x),
        y = as.numeric(sp$y)
      )
    }
  }

  labels <- list()
  ce <- geom$centers
  if (!is.null(ce) && NROW(ce) > 0L) {
    has_place <- "kind" %in% names(ce)
    for (r in seq_len(NROW(ce))) {
      parts <- c(ce$labels[r], ce$quantities[r], ce$annotations[r])
      parts <- parts[!is.na(parts)]
      if (length(parts) == 0L) {
        next
      }
      is_q <- !is.na(ce$quantities[r])
      leader <- NULL
      if (has_place && nzchar(ce$kind[r]) && ce$kind[r] != "interior") {
        wp <- ce$leader_waypoints[[r]]
        leader <- list(
          x0 = ce$tether_x[r],
          y0 = ce$tether_y[r],
          x1 = ce$leader_end_x[r],
          y1 = ce$leader_end_y[r],
          wx = as.numeric(wp$x),
          wy = as.numeric(wp$y)
        )
      }
      labels[[length(labels) + 1L]] <- list(
        x = ce$x[r],
        y = ce$y[r],
        text = paste(parts, collapse = "\n"),
        col = to_hex(if (is_q) opar$quantities$col else opar$labels$col),
        fontsize = if (is_q) opar$quantities$fontsize else opar$labels$fontsize,
        leader = leader
      )
    }
  }

  container <- NULL
  if (do_complement && !is.null(geom$container)) {
    cd <- geom$container
    cp <- cd$complement_polygon
    container <- list(
      outline_x = as.numeric(cd$outline$x),
      outline_y = as.numeric(cd$outline$y),
      complement_x = as.numeric(cp$x),
      complement_y = as.numeric(cp$y),
      complement_id_lengths = as.integer(cp$id_lengths),
      fill = to_hex(opar$complement$fill),
      alpha = opar$complement$alpha,
      col = to_hex(opar$complement$col),
      lwd = opar$complement$lwd,
      lty = opar$complement$lty,
      label_x = cd$label_x,
      label_y = cd$label_y,
      label_text = if (is.na(cd$quantity_label)) NULL else cd$quantity_label,
      fontsize = opar$complement$fontsize,
      label_col = to_hex(opar$complement$col)
    )
  }

  list(
    regions = unname(regions),
    edges = unname(edges),
    labels = unname(labels),
    container = container,
    xlim = as.numeric(geom$xlim),
    ylim = as.numeric(geom$ylim)
  )
}

#' Format a quantity value for a tooltip, matching the static plot's counts
#' @keywords internal
#' @noRd
format_quantity <- function(x) {
  as.character(signif(x, digits = getOption("digits")))
}

#' Convert any R color specification to a CSS hex string
#' @keywords internal
#' @noRd
to_hex <- function(col) {
  if (is.null(col)) {
    return(NULL)
  }
  is_transparent <- is.na(col) |
    (is.character(col) & tolower(col) == "transparent")
  rgb <- grDevices::col2rgb(ifelse(is_transparent, "white", col))
  out <- grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
  out[is_transparent] <- "transparent"
  out
}

#' Error early if Suggested packages needed by a feature are missing
#' @keywords internal
#' @noRd
check_suggests <- function(pkgs) {
  missing <- pkgs[!vapply(
    pkgs,
    function(p) requireNamespace(p, quietly = TRUE),
    logical(1)
  )]
  if (length(missing) > 0L) {
    stop(
      "`euler_widget()` requires the ",
      paste(sprintf("'%s'", missing), collapse = ", "),
      " package(s). Install with install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      ")).",
      call. = FALSE
    )
  }
}
