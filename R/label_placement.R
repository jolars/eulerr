#' Default placement options used when the caller doesn't supply any.
#'
#' Mirrors `eunoia::plotting::PlacementStrategy::default()`: raycast
#' exterior solver, POI tether, proportional margin / iterations.
#' @keywords internal
default_placement_opts <- function() {
  list(
    placement = "raycast",
    margin = NULL,
    iterations = NULL,
    tether = "poi",
    gap = NULL
  )
}

#' Merge user-supplied placement options onto the defaults.
#' @keywords internal
resolve_placement_opts <- function(opts) {
  if (is.null(opts)) {
    return(default_placement_opts())
  }
  defaults <- default_placement_opts()
  for (key in names(defaults)) {
    val <- opts[[key]]
    if (!is.null(val)) {
      defaults[[key]] <- val
    }
  }
  defaults
}

#' Open a temporary grid measurement device + viewport.
#'
#' Returns an idempotent closer thunk that pops the viewport and
#' closes the null PDF device. Used so we can call
#' `grid::convertWidth(grobWidth(...), "native", ...)` at setup time —
#' i.e. before `plot.eulergram()` ever opens a real device — to size
#' the label boxes that drive label placement.
#'
#' Always opens its own off-screen PDF rather than reusing the caller's
#' active device. Pushing a viewport onto the caller's device adds an
#' entry to its display list, which knitr/evaluate's plot capture
#' treats as visible change and emits as an extra blank plot before
#' the real `plot.eulergram()` draws. The off-screen PDF keeps
#' measurement entirely out of the user's display list.
#' @keywords internal
open_measurement_viewport <- function(xlim, ylim) {
  # Setup-time placement just sizes the *plot region*; draw-time
  # `makeContent.EulerTags` re-places labels against the live device.
  # The measurement device picks a deliberately small reference
  # (3 × 3 in) so the setup-time canvas bbox is conservative enough
  # that exterior-label anchors at typical interactive device sizes
  # still land inside `xlim`/`ylim` after a resize.
  #
  # The measurement viewport must also preserve the diagram's aspect
  # (it's recreated by `plot.eulergram()` via `grid.layout(respect =
  # TRUE)` at draw time). Without aspect preservation the setup-time
  # viewport stretches one axis, so 1 pt in y maps to a smaller native
  # extent than at draw time — measured heights come back too small,
  # the canvas bbox isn't widened enough, and exterior labels overrun
  # `ylim` once the user resizes the device.
  prev_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL, width = 7, height = 7)
  meas_dev <- grDevices::dev.cur()
  xrng <- diff(xlim)
  yrng <- diff(ylim)
  if (!is.finite(xrng) || !is.finite(yrng) || xrng <= 0 || yrng <= 0) {
    width <- grid::unit(1, "snpc")
    height <- grid::unit(1, "snpc")
  } else if (xrng >= yrng) {
    width <- grid::unit(1, "snpc")
    height <- grid::unit(yrng / xrng, "snpc")
  } else {
    width <- grid::unit(xrng / yrng, "snpc")
    height <- grid::unit(1, "snpc")
  }
  grid::pushViewport(grid::viewport(
    width = width,
    height = height,
    xscale = xlim,
    yscale = ylim
  ))
  closed <- FALSE
  function() {
    if (closed) {
      return(invisible(NULL))
    }
    closed <<- TRUE
    grid::popViewport()
    grDevices::dev.off(meas_dev)
    if (prev_dev != 1L) {
      grDevices::dev.set(prev_dev)
    }
  }
}

#' Native-unit AABB of one composite tag (label stacked above quantity,
#' annotation stacked below quantity, separated by `padding`). The
#' geometry matches what `setup_tag()` renders at draw time, so the size
#' handed to eunoia agrees with the actual on-screen footprint.
#' @keywords internal
measure_tag <- function(
  label,
  quantity,
  annotation,
  labels_par_id,
  quantities_par_id,
  annotations_par_id,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding_native
) {
  # `*_par_id` are only assigned for drawable tags, so they're the
  # authoritative "is there something to draw" signal. Don't call
  # `is.na(label)` on the text — `label` may be an expression (from
  # `str2expression(...)`) and `is.na` warns there.
  do_label <- !is.null(labels_gp) &&
    !is.null(labels_par_id) &&
    !is.na(labels_par_id) &&
    !is.null(label)
  do_quant <- !is.null(quantities_gp) &&
    !is.null(quantities_par_id) &&
    !is.na(quantities_par_id) &&
    !is.null(quantity)
  do_annot <- !is.null(annotations_gp) &&
    !is.null(annotations_par_id) &&
    !is.na(annotations_par_id) &&
    !is.null(annotation)
  if (do_label && !is.expression(label) && is.na(label)) {
    do_label <- FALSE
  }
  if (do_quant && !is.expression(quantity) && is.na(quantity)) {
    do_quant <- FALSE
  }
  if (do_annot && !is.expression(annotation) && is.na(annotation)) {
    do_annot <- FALSE
  }

  if (!do_label && !do_quant && !do_annot) {
    return(list(w = 0, h = 0))
  }

  measure_one <- function(text, gp_row) {
    g <- grid::textGrob(text, gp = gp_row)
    list(
      w = grid::convertWidth(
        grid::grobWidth(g),
        "native",
        valueOnly = TRUE
      ),
      h = grid::convertHeight(
        grid::grobHeight(g),
        "native",
        valueOnly = TRUE
      )
    )
  }

  label_w <- 0
  label_h <- 0
  quant_w <- 0
  quant_h <- 0
  annot_w <- 0
  annot_h <- 0

  if (do_label) {
    m <- measure_one(label, labels_gp[labels_par_id])
    label_w <- m$w
    label_h <- m$h
  }
  if (do_quant) {
    m <- measure_one(quantity, quantities_gp[quantities_par_id])
    quant_w <- m$w
    quant_h <- m$h
  }
  if (do_annot) {
    m <- measure_one(annotation, annotations_gp[annotations_par_id])
    annot_w <- m$w
    annot_h <- m$h
  }

  pad_lq <- if (do_label && do_quant) padding_native else 0
  pad_qa <- if (do_quant && do_annot) padding_native else 0
  pad_la <- if (!do_quant && do_label && do_annot) padding_native else 0
  list(
    w = max(label_w, quant_w, annot_w),
    h = label_h + quant_h + annot_h + pad_lq + pad_qa + pad_la
  )
}

#' Resolve a `gap` option to a numeric in native units inside the current
#' measurement viewport. `NULL` → falls back to `padding_native` so the
#' visible leader-tip gap matches the spacing between label and quantity.
#' A `grid::unit` value converts to native; a bare numeric is interpreted
#' as `lines` (same convention as `eulerr_options()$padding`).
#' @keywords internal
resolve_gap_native <- function(gap, padding_native) {
  if (is.null(gap)) {
    return(padding_native)
  }
  if (inherits(gap, "unit")) {
    return(grid::convertHeight(gap, "native", valueOnly = TRUE))
  }
  grid::convertHeight(grid::unit(gap, "lines"), "native", valueOnly = TRUE)
}

#' Measure all candidate tag sizes (regions + optional complement) inside
#' a fresh measurement viewport scaled to `xlim`/`ylim`.
#' @keywords internal
measure_tag_sizes <- function(
  centers,
  do_complement_label,
  complement_label,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding,
  gap,
  xlim,
  ylim
) {
  close_vp <- open_measurement_viewport(xlim, ylim)
  on.exit(close_vp(), add = TRUE)

  padding_native <- grid::convertHeight(padding, "native", valueOnly = TRUE)
  gap_native <- resolve_gap_native(gap, padding_native)

  n_rows <- if (is.null(centers)) 0L else NROW(centers)
  widths <- numeric(0)
  heights <- numeric(0)
  combos <- character(0)

  has_annotations_col <- !is.null(centers) &&
    "annotations" %in% names(centers)

  if (n_rows > 0L) {
    widths <- numeric(n_rows)
    heights <- numeric(n_rows)
    combos <- rownames(centers)
    for (i in seq_len(n_rows)) {
      annotation_text <- if (has_annotations_col) centers$annotations[i] else NA
      annotation_par_id <- if (has_annotations_col) {
        centers$annotations_par_id[i]
      } else {
        NA_integer_
      }
      m <- measure_tag(
        label = centers$labels[i],
        quantity = centers$quantities[i],
        annotation = annotation_text,
        labels_par_id = centers$labels_par_id[i],
        quantities_par_id = centers$quantities_par_id[i],
        annotations_par_id = annotation_par_id,
        labels_gp = labels_gp,
        quantities_gp = quantities_gp,
        annotations_gp = annotations_gp,
        padding_native = padding_native
      )
      widths[i] <- m$w
      heights[i] <- m$h
    }
  }

  if (do_complement_label && !is.null(complement_label) && !is.na(complement_label)) {
    # The complement label is just text — there's no per-set label or
    # annotation, so measure as if only the quantity is drawn.
    m <- measure_tag(
      label = NA,
      quantity = complement_label,
      annotation = NA,
      labels_par_id = NA_integer_,
      quantities_par_id = 1L,
      annotations_par_id = NA_integer_,
      labels_gp = NULL,
      quantities_gp = quantities_gp,
      annotations_gp = NULL,
      padding_native = padding_native
    )
    widths <- c(widths, m$w)
    heights <- c(heights, m$h)
    combos <- c(combos, "")
  }

  list(
    combos = combos,
    widths = widths,
    heights = heights,
    gap_native = gap_native
  )
}

#' Single placement pass: measure tags, call the Rust FFI, return the
#' placement records and the canvas bbox returned by eunoia.
#' @keywords internal
run_placement_pass <- function(
  centers,
  container_data,
  ellipses,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding,
  placement_opts,
  do_complement_label,
  xlim,
  ylim,
  n_vertices,
  label_precision
) {
  sizes <- measure_tag_sizes(
    centers = centers,
    do_complement_label = do_complement_label,
    complement_label = if (do_complement_label) container_data$quantity_label else NA,
    labels_gp = labels_gp,
    quantities_gp = quantities_gp,
    annotations_gp = annotations_gp,
    padding = padding,
    gap = placement_opts$gap,
    xlim = xlim,
    ylim = ylim
  )

  if (length(sizes$combos) == 0L) {
    return(list(sizes = sizes, placements = NULL))
  }

  do_container <- !is.null(container_data)
  placements <- place_euler_labels(
    set_names = rownames(ellipses),
    h = ellipses$h,
    k = ellipses$k,
    a = ellipses$a,
    b = ellipses$b,
    phi = ellipses$phi,
    container_h = if (do_container) container_data$h else NULL,
    container_k = if (do_container) container_data$k else NULL,
    container_width = if (do_container) container_data$width else NULL,
    container_height = if (do_container) container_data$height else NULL,
    n_vertices = as.integer(n_vertices),
    label_combos = sizes$combos,
    label_widths = sizes$widths,
    label_heights = sizes$heights,
    placement = placement_opts$placement,
    placement_margin = placement_opts$margin,
    placement_iterations = placement_opts$iterations,
    placement_tether = placement_opts$tether,
    placement_leader_gap = sizes$gap_native,
    label_precision = label_precision
  )

  list(sizes = sizes, placements = placements)
}

#' Union the current `xlim`/`ylim` with the canvas bbox reported by
#' eunoia. Returns the (possibly widened) limits.
#'
#' `slack` pads the canvas bbox before the union (multiplicative,
#' centered on the bbox center). The setup-time bbox is sized for one
#' reference device; padding gives the draw-time placement headroom
#' when the user resizes the device smaller than that reference and
#' labels grow in native units accordingly. A `slack` of 1.4 absorbs
#' roughly a 30 % linear resize before exterior labels start to fall
#' outside the panel viewport.
#' @keywords internal
expand_limits_with_canvas <- function(limits, placements, slack = 1.4) {
  if (
    is.null(placements) ||
      !is.finite(placements$canvas_bbox_h) ||
      !is.finite(placements$canvas_bbox_k) ||
      !is.finite(placements$canvas_bbox_width) ||
      !is.finite(placements$canvas_bbox_height) ||
      placements$canvas_bbox_width <= 0 ||
      placements$canvas_bbox_height <= 0
  ) {
    return(limits)
  }
  cx_half <- placements$canvas_bbox_width * slack / 2
  cy_half <- placements$canvas_bbox_height * slack / 2
  canvas_xlim <- c(
    placements$canvas_bbox_h - cx_half,
    placements$canvas_bbox_h + cx_half
  )
  canvas_ylim <- c(
    placements$canvas_bbox_k - cy_half,
    placements$canvas_bbox_k + cy_half
  )
  list(
    xlim = range(c(limits$xlim, canvas_xlim)),
    ylim = range(c(limits$ylim, canvas_ylim))
  )
}

#' Run eunoia label placement, expanding limits so exterior labels are
#' not clipped. Drives one initial pass plus one re-measure pass when
#' the limits widened by more than `re_measure_threshold` on the short
#' side. Updates `centers` (and the complement slot on `container_data`)
#' in place with placed `(x, y)` plus `kind`, `tether_x`, `tether_y`,
#' `leader_end_x`, `leader_end_y`.
#'
#' When `placement_opts` is `NULL`, defaults to eunoia's raycast + POI
#' tether.
#'
#' Returns a list with `centers`, `container_data`, and `limits`.
#' @keywords internal
apply_label_placement <- function(
  centers,
  container_data,
  ellipses,
  labels,
  quantities,
  annotations = NULL,
  placement_opts = NULL,
  do_complement_label = FALSE,
  limits,
  n_vertices,
  label_precision,
  re_measure_threshold = 0.01
) {
  no_centers <- is.null(centers) || NROW(centers) == 0L
  if (no_centers && !do_complement_label) {
    return(list(
      centers = centers,
      container_data = container_data,
      limits = limits
    ))
  }
  if (NROW(ellipses) == 0L) {
    return(list(
      centers = centers,
      container_data = container_data,
      limits = limits
    ))
  }

  placement_opts <- resolve_placement_opts(placement_opts)
  labels_gp <- if (!is.null(labels)) labels$gp else NULL
  quantities_gp <- if (!is.null(quantities)) quantities$gp else NULL
  annotations_gp <- if (!is.null(annotations)) annotations$gp else NULL
  padding <- eulerr_options()$padding

  # First pass at the incoming limits. The aspect-preserving
  # measurement viewport's native-per-pt ratio is set by `xrng/yrng`,
  # so widening the bbox between passes changes the measurement and
  # the iteration can diverge for very large labels relative to the
  # diagram. One re-measure is enough for typical cases and converges
  # for everything we expect to draw correctly at the reference
  # device size.
  pass1 <- run_placement_pass(
    centers = centers,
    container_data = container_data,
    ellipses = ellipses,
    labels_gp = labels_gp,
    quantities_gp = quantities_gp,
    annotations_gp = annotations_gp,
    padding = padding,
    placement_opts = placement_opts,
    do_complement_label = do_complement_label,
    xlim = limits$xlim,
    ylim = limits$ylim,
    n_vertices = n_vertices,
    label_precision = label_precision
  )
  if (is.null(pass1$placements)) {
    return(list(
      centers = centers,
      container_data = container_data,
      limits = limits
    ))
  }
  new_limits <- expand_limits_with_canvas(limits, pass1$placements)
  final <- pass1

  kinds <- pass1$placements$kind
  has_exterior <- any(nzchar(kinds) & kinds != "interior")
  old_short <- min(diff(limits$xlim), diff(limits$ylim))
  new_short <- min(diff(new_limits$xlim), diff(new_limits$ylim))
  if (has_exterior && is.finite(old_short) && old_short > 0) {
    rel_change <- abs(new_short - old_short) / old_short
    if (rel_change > re_measure_threshold) {
      pass2 <- run_placement_pass(
        centers = centers,
        container_data = container_data,
        ellipses = ellipses,
        labels_gp = labels_gp,
        quantities_gp = quantities_gp,
        annotations_gp = annotations_gp,
        padding = padding,
        placement_opts = placement_opts,
        do_complement_label = do_complement_label,
        xlim = new_limits$xlim,
        ylim = new_limits$ylim,
        n_vertices = n_vertices,
        label_precision = label_precision
      )
      if (!is.null(pass2$placements)) {
        new_limits <- expand_limits_with_canvas(new_limits, pass2$placements)
        final <- pass2
      }
    }
  }

  # Apply placements back to centers and container_data.
  combos <- final$sizes$combos
  pl <- final$placements

  centers_combos <- if (!no_centers) rownames(centers) else character(0)
  if (!no_centers) {
    centers$kind <- rep("", NROW(centers))
    centers$tether_x <- rep(NA_real_, NROW(centers))
    centers$tether_y <- rep(NA_real_, NROW(centers))
    centers$leader_end_x <- rep(NA_real_, NROW(centers))
    centers$leader_end_y <- rep(NA_real_, NROW(centers))
    if (length(centers_combos) > 0L) {
      idx <- match(centers_combos, combos)
      ok <- !is.na(idx)
      if (any(ok)) {
        ax <- pl$anchor_x[idx[ok]]
        ay <- pl$anchor_y[idx[ok]]
        kx <- pl$kind[idx[ok]]
        tx <- pl$tether_x[idx[ok]]
        ty <- pl$tether_y[idx[ok]]
        lex <- pl$leader_end_x[idx[ok]]
        ley <- pl$leader_end_y[idx[ok]]
        # Keep the POI fallback when eunoia returned NA / no placement.
        valid <- is.finite(ax) & is.finite(ay)
        rows <- which(ok)[valid]
        centers$x[rows] <- ax[valid]
        centers$y[rows] <- ay[valid]
        centers$kind[rows] <- kx[valid]
        centers$tether_x[rows] <- tx[valid]
        centers$tether_y[rows] <- ty[valid]
        centers$leader_end_x[rows] <- lex[valid]
        centers$leader_end_y[rows] <- ley[valid]
      }
    }
  }

  if (do_complement_label && !is.null(container_data)) {
    idx <- match("", combos)
    if (!is.na(idx)) {
      ax <- pl$anchor_x[idx]
      ay <- pl$anchor_y[idx]
      if (is.finite(ax) && is.finite(ay)) {
        container_data$label_x <- ax
        container_data$label_y <- ay
        container_data$kind <- pl$kind[idx]
        container_data$tether_x <- pl$tether_x[idx]
        container_data$tether_y <- pl$tether_y[idx]
        container_data$leader_end_x <- pl$leader_end_x[idx]
        container_data$leader_end_y <- pl$leader_end_y[idx]
      }
    }
  }

  list(
    centers = centers,
    container_data = container_data,
    limits = new_limits
  )
}
