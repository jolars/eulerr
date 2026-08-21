#' Validate glyph placement controls
#'
#' @param x normalized glyph options.
#' @return `NULL`, invisibly.
#' @keywords internal
validate_glyph_options <- function(x) {
  scalar_number <- function(value, name, lower = -Inf, integer = FALSE) {
    if (
      length(value) != 1L ||
        !is.numeric(value) ||
        !is.finite(value) ||
        value < lower ||
        (integer && value != as.integer(value))
    ) {
      qualifier <- if (integer) "integer" else "number"
      stop("`glyphs$", name, "` must be a finite ", qualifier, ".")
    }
  }
  scalar_number(x$gap, "gap", 0)
  scalar_number(x$seed, "seed", 0, TRUE)
  scalar_number(x$max_attempts, "max_attempts", 1, TRUE)
  scalar_number(x$min_scale, "min_scale", .Machine$double.eps)
  if (x$min_scale > 1) {
    stop("`glyphs$min_scale` must not exceed 1.")
  }
  if (!is.null(x$radius)) {
    scalar_number(x$radius, "radius", .Machine$double.eps)
  }
  if (!is.null(x$scale)) {
    scalar_number(x$scale, "scale", .Machine$double.eps)
  }
  if (!is.null(x$max_items)) {
    scalar_number(x$max_items, "max_items", 1, TRUE)
  }
  if (identical(x$mode, "members")) {
    if (!is.list(x$labels) || is.null(names(x$labels))) {
      stop(
        "Member glyphs require `glyphs$labels` as a fully named list ",
        "keyed by region."
      )
    }
    if (!all(vapply(x$labels, is.character, logical(1)))) {
      stop("Every element of `glyphs$labels` must be a character vector.")
    }
  }
  invisible(NULL)
}

canonical_region_name <- function(x, set_names) {
  if (identical(x, "")) {
    return("")
  }
  parts <- strsplit(x, "&", fixed = TRUE)[[1L]]
  if (length(parts) == 0L || any(!nzchar(parts)) || anyDuplicated(parts)) {
    stop("Invalid glyph region name: `", x, "`.")
  }
  unknown <- setdiff(parts, set_names)
  if (length(unknown) > 0L) {
    stop("Unknown set in glyph region `", x, "`: ", unknown[1L], ".")
  }
  paste(set_names[set_names %in% parts], collapse = "&")
}

canonicalize_region_values <- function(x, set_names, what) {
  nms <- names(x)
  if (is.null(nms)) {
    stop("`glyphs$", what, "` must be fully named by region.")
  }
  canonical <- vapply(nms, canonical_region_name, character(1), set_names)
  if (anyDuplicated(canonical)) {
    stop("`glyphs$", what, "` contains duplicate regions.")
  }
  names(x) <- canonical
  x
}

prepare_glyph_counts <- function(glyphs, data) {
  counts <- glyphs$counts
  if (is.null(counts)) {
    counts <- data$original.values
    if (!is.null(data$container) && is.finite(data$container$complement)) {
      counts <- c(counts, stats::setNames(data$container$complement, ""))
    }
  } else {
    if (!is.numeric(counts)) {
      stop("`glyphs$counts` must be numeric.")
    }
    counts <- canonicalize_region_values(
      counts,
      rownames(data$shapes),
      "counts"
    )
  }
  if (
    any(!is.finite(counts)) || any(counts < 0) || any(counts != round(counts))
  ) {
    stop("Glyph counts must be finite, nonnegative integers.")
  }
  counts <- counts[counts > 0]
  max_items <- glyphs$max_items %||% 2000L
  if (sum(counts) > max_items) {
    warning(
      "Glyph rendering skipped: ",
      sum(counts),
      " dots exceed `glyphs$max_items` (",
      max_items,
      ")."
    )
    return(numeric())
  }
  counts
}

prepare_member_labels <- function(glyphs, data) {
  labels <- canonicalize_region_values(
    glyphs$labels,
    rownames(data$shapes),
    "labels"
  )
  labels <- labels[lengths(labels) > 0L]
  n <- sum(lengths(labels))
  max_items <- glyphs$max_items %||% 500L
  if (n > max_items) {
    warning(
      "Member glyph rendering skipped: ",
      n,
      " labels exceed `glyphs$max_items` (",
      max_items,
      ")."
    )
    return(list())
  }
  labels
}

euler_shape_args <- function(x, n_vertices) {
  shapes <- x$shapes
  container <- x$container
  list(
    set_names = rownames(shapes),
    shape = if (NROW(shapes)) shapes$type[1L] else "ellipse",
    h = shapes$h,
    k = shapes$k,
    a = shapes$a,
    b = shapes$b,
    phi = shapes$phi,
    width = shapes$width,
    height = shapes$height,
    side = shapes$side,
    container_h = container$h %||% NULL,
    container_k = container$k %||% NULL,
    container_width = container$width %||% NULL,
    container_height = container$height %||% NULL,
    n_vertices = as.integer(n_vertices)
  )
}

empty_obstacles <- function() {
  list(h = numeric(), k = numeric(), width = numeric(), height = numeric())
}

#' Build draw-time exterior set labels
#' @keywords internal
setup_euler_set_labels <- function(x, labels, n_vertices, tags = NULL, number) {
  if (NROW(x$shapes) == 0L) {
    return(NULL)
  }
  children <- grid::gList()
  for (i in seq_len(NROW(x$shapes))) {
    children[[i]] <- grid::textGrob(
      labels$labels[i],
      x = grid::unit(x$shapes$h[i], "native"),
      y = grid::unit(x$shapes$k[i], "native"),
      rot = labels$rot[i],
      gp = labels$gp[i],
      name = paste0("set.label.grob.", number, ".", i)
    )
  }
  grid::gTree(
    children = children,
    shape_args = euler_shape_args(x, n_vertices),
    margin = labels$margin,
    angular_steps = labels$angular_steps,
    precision = max(diff(x$xlim), diff(x$ylim)) / 100,
    tags = tags,
    name = paste0("set.labels.", number),
    cl = "EulerSetLabels"
  )
}

place_set_label_tree <- function(x) {
  n <- length(x$children)
  widths <- heights <- numeric(n)
  for (i in seq_len(n)) {
    widths[i] <- grid::convertWidth(
      grid::grobWidth(x$children[[i]]),
      "native",
      valueOnly = TRUE
    )
    heights[i] <- grid::convertHeight(
      grid::grobHeight(x$children[[i]]),
      "native",
      valueOnly = TRUE
    )
  }
  obstacles <- if (is.null(x$tags)) {
    empty_obstacles()
  } else {
    glyph_obstacles(list(tags = x$tags, set_labels = NULL))
  }
  do.call(
    place_euler_set_labels,
    c(
      x$shape_args,
      list(
        label_widths = widths,
        label_heights = heights,
        obstacle_h = obstacles$h,
        obstacle_k = obstacles$k,
        obstacle_width = obstacles$width,
        obstacle_height = obstacles$height,
        margin = x$margin,
        angular_steps = as.integer(x$angular_steps),
        precision = x$precision
      )
    )
  )
}

#' Re-place exterior set labels on the active device
#' @export
#' @keywords internal
makeContent.EulerSetLabels <- function(x) {
  placed <- place_set_label_tree(x)
  for (i in seq_along(x$children)) {
    if (is.finite(placed$anchor_x[i]) && is.finite(placed$anchor_y[i])) {
      x$children[[i]]$x <- grid::unit(placed$anchor_x[i], "native")
      x$children[[i]]$y <- grid::unit(placed$anchor_y[i], "native")
    }
  }
  x
}

#' Build a draw-time glyph layer
#' @keywords internal
setup_euler_glyphs <- function(
  x,
  glyphs,
  n_vertices,
  combo_labels,
  tags = NULL,
  set_labels = NULL,
  number
) {
  if (NROW(x$shapes) == 0L) {
    return(NULL)
  }
  mode <- glyphs$mode
  values <- if (identical(mode, "dots")) {
    prepare_glyph_counts(glyphs, x)
  } else {
    prepare_member_labels(glyphs, x)
  }
  if (length(values) == 0L) {
    return(NULL)
  }
  grid::gTree(
    children = grid::gList(),
    shape_args = euler_shape_args(x, n_vertices),
    mode = mode,
    values = values,
    options = glyphs,
    combo_labels = combo_labels,
    tags = tags,
    set_labels = set_labels,
    precision = max(diff(x$xlim), diff(x$ylim)) / 100,
    name = paste0("glyphs.", number),
    cl = "EulerGlyphs"
  )
}

glyph_obstacles <- function(x) {
  out <- empty_obstacles()
  pad <- grid::convertHeight(
    eulerr_options()$padding,
    "native",
    valueOnly = TRUE
  )
  if (!is.null(x$tags) && length(x$tags$children) > 0L) {
    measured <- measure_all_tags(
      x$tags,
      x$tags$padding,
      x$tags$placement_opts$gap
    )
    if (length(measured$combos) > 0L) {
      args <- list(
        set_names = rownames(x$tags$shapes),
        shape = x$tags$shapes$type[1L],
        h = x$tags$shapes$h,
        k = x$tags$shapes$k,
        a = x$tags$shapes$a,
        b = x$tags$shapes$b,
        phi = x$tags$shapes$phi,
        width = x$tags$shapes$width,
        height = x$tags$shapes$height,
        side = x$tags$shapes$side,
        container_h = x$tags$container$h %||% NULL,
        container_k = x$tags$container$k %||% NULL,
        container_width = x$tags$container$width %||% NULL,
        container_height = x$tags$container$height %||% NULL,
        n_vertices = as.integer(x$tags$n_vertices),
        label_combos = measured$combos,
        label_widths = measured$widths,
        label_heights = measured$heights,
        placement = x$tags$placement_opts$placement %||% "raycast",
        placement_margin = x$tags$placement_opts$margin,
        placement_iterations = x$tags$placement_opts$iterations,
        placement_min_gap = x$tags$placement_opts$min_gap,
        placement_tether = x$tags$placement_opts$tether %||% "poi",
        placement_leader_gap = measured$gap_native,
        label_precision = x$tags$label_precision
      )
      placed <- do.call(place_euler_labels, args)
      ok <- is.finite(placed$anchor_x) & is.finite(placed$anchor_y)
      out$h <- c(out$h, placed$anchor_x[ok])
      out$k <- c(out$k, placed$anchor_y[ok])
      out$width <- c(out$width, measured$widths[ok] + 2 * pad)
      out$height <- c(out$height, measured$heights[ok] + 2 * pad)
    }
  }
  if (!is.null(x$set_labels) && length(x$set_labels$children) > 0L) {
    widths <- heights <- numeric(length(x$set_labels$children))
    for (i in seq_along(x$set_labels$children)) {
      widths[i] <- grid::convertWidth(
        grid::grobWidth(x$set_labels$children[[i]]),
        "native",
        valueOnly = TRUE
      )
      heights[i] <- grid::convertHeight(
        grid::grobHeight(x$set_labels$children[[i]]),
        "native",
        valueOnly = TRUE
      )
    }
    placed <- place_set_label_tree(x$set_labels)
    ok <- is.finite(placed$anchor_x) & is.finite(placed$anchor_y)
    out$h <- c(out$h, placed$anchor_x[ok])
    out$k <- c(out$k, placed$anchor_y[ok])
    out$width <- c(out$width, widths[ok] + 2 * pad)
    out$height <- c(out$height, heights[ok] + 2 * pad)
  }
  out
}

split_flat_values <- function(x, lengths) {
  if (length(lengths) == 0L) {
    return(list())
  }
  ends <- cumsum(lengths)
  starts <- c(1L, utils::head(ends + 1L, -1L))
  Map(
    function(start, end) {
      if (end < start) numeric() else x[seq.int(start, end)]
    },
    starts,
    ends
  )
}

glyph_region_gp <- function(x, region) {
  i <- match(region, x$combo_labels)
  if (is.na(i)) {
    i <- 1L
  }
  x$options$gp[i]
}

#' Place and render glyphs on the active device
#' @export
#' @keywords internal
makeContent.EulerGlyphs <- function(x) {
  obstacles <- glyph_obstacles(x)
  opts <- x$options
  if (identical(x$mode, "dots")) {
    placed <- do.call(
      place_euler_glyphs,
      c(
        x$shape_args,
        list(
          region_names = names(x$values),
          counts = as.integer(x$values),
          arrangement = opts$arrangement,
          radius = opts$radius,
          gap = opts$gap,
          seed = opts$seed,
          max_attempts = as.integer(opts$max_attempts),
          obstacle_h = obstacles$h,
          obstacle_k = obstacles$k,
          obstacle_width = obstacles$width,
          obstacle_height = obstacles$height,
          precision = x$precision
        )
      )
    )
    points_x <- split_flat_values(placed$x, placed$id_lengths)
    points_y <- split_flat_values(placed$y, placed$id_lengths)
    children <- grid::gList()
    for (i in seq_along(x$values)) {
      if (placed$id_lengths[i] > 0L) {
        children[[length(children) + 1L]] <- grid::circleGrob(
          x = points_x[[i]],
          y = points_y[[i]],
          r = grid::unit(placed$radius, "native"),
          default.units = "native",
          gp = glyph_region_gp(x, names(x$values)[i]),
          name = paste0("glyph.discs.", i)
        )
      }
    }
    if (sum(placed$unplaced) > 0L) {
      warning(sum(placed$unplaced), " glyphs could not be placed.")
    }
    return(grid::setChildren(x, children))
  }

  item_regions <- rep(names(x$values), lengths(x$values))
  item_labels <- unlist(x$values, use.names = FALSE)
  item_widths <- item_heights <- numeric(length(item_labels))
  for (i in seq_along(item_labels)) {
    gp <- glyph_region_gp(x, item_regions[i])
    probe <- grid::textGrob(item_labels[i], gp = gp)
    item_widths[i] <- grid::convertWidth(
      grid::grobWidth(probe),
      "native",
      valueOnly = TRUE
    )
    item_heights[i] <- grid::convertHeight(
      grid::grobHeight(probe),
      "native",
      valueOnly = TRUE
    )
  }
  placed <- do.call(
    place_euler_glyph_boxes,
    c(
      x$shape_args,
      list(
        item_regions = item_regions,
        item_widths = item_widths,
        item_heights = item_heights,
        arrangement = opts$arrangement,
        scale = opts$scale,
        min_scale = opts$min_scale,
        gap = opts$gap,
        seed = opts$seed,
        max_attempts = as.integer(opts$max_attempts),
        obstacle_h = obstacles$h,
        obstacle_k = obstacles$k,
        obstacle_width = obstacles$width,
        obstacle_height = obstacles$height,
        precision = x$precision
      )
    )
  )
  h <- split_flat_values(placed$h, placed$id_lengths)
  k <- split_flat_values(placed$k, placed$id_lengths)
  children <- grid::gList()
  value_index <- 0L
  for (i in seq_along(placed$region_names)) {
    region <- placed$region_names[i]
    labels <- x$values[[region]]
    n_placed <- placed$id_lengths[i]
    if (n_placed == 0L) {
      next
    }
    for (j in seq_len(n_placed)) {
      value_index <- value_index + 1L
      gp <- glyph_region_gp(x, region)
      gp$fontsize <- gp$fontsize * placed$scale
      children[[length(children) + 1L]] <- grid::textGrob(
        labels[j],
        x = h[[i]][j],
        y = k[[i]][j],
        default.units = "native",
        gp = gp,
        name = paste0("glyph.member.", value_index)
      )
    }
  }
  if (sum(placed$unplaced) > 0L) {
    warning(sum(placed$unplaced), " member labels could not be placed.")
  }
  grid::setChildren(x, children)
}
