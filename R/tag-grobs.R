#' Build the leader / label / quantity / annotation gList for one tag.
#'
#' Shared between [setup_tag()] (initial construction in [setup_grobs()])
#' and [makeContent.EulerTags()] (draw-time re-placement on resize).
#' Pure factory — no measurement; takes anchor + tether already in
#' native units, plus the stashed text / gpar bundle. The label /
#' quantity / annotation stack is centered vertically on `(ax, ay)`
#' so the bbox center matches the anchor eunoia placed and leader
#' endpoints land on the actual bbox edge.
#'
#' @keywords internal
build_tag_grobs <- function(
  ax,
  ay,
  kind,
  tx,
  ty,
  lend_x,
  lend_y,
  label_text,
  quantity_text,
  annotation_text,
  has_label,
  has_quantity,
  has_annotation,
  label_gp,
  quantity_gp,
  annotation_gp,
  label_rot,
  quantity_rot,
  annotation_rot,
  number,
  leader_gp_list,
  padding,
  waypoints_x = numeric(0),
  waypoints_y = numeric(0),
  name_prefix = "tag"
) {
  qname <- if (identical(name_prefix, "complement")) {
    "complement.quantity.grob"
  } else {
    paste0(name_prefix, ".quantity.", number)
  }
  lname <- if (identical(name_prefix, "complement")) {
    "complement.label.grob"
  } else {
    paste0(name_prefix, ".label.", number)
  }
  aname <- if (identical(name_prefix, "complement")) {
    "complement.annotation.grob"
  } else {
    paste0(name_prefix, ".annotation.", number)
  }
  leader_name <- if (identical(name_prefix, "complement")) {
    "complement.leader.grob"
  } else {
    paste0(name_prefix, ".leader.", number)
  }

  ax_unit <- grid::unit(ax, "native")
  ay_unit <- grid::unit(ay, "native")

  if (has_quantity) {
    quantity_grob <- grid::textGrob(
      quantity_text,
      x = ax_unit,
      y = ay_unit,
      rot = quantity_rot,
      gp = quantity_gp,
      name = qname
    )
  } else {
    quantity_grob <- grid::nullGrob(name = paste0(qname, ".null"))
  }

  if (has_label) {
    label_grob <- grid::textGrob(
      label_text,
      x = ax_unit,
      y = ay_unit,
      rot = label_rot,
      gp = label_gp,
      name = lname
    )
  } else {
    label_grob <- grid::nullGrob(name = paste0(lname, ".null"))
  }

  if (has_annotation) {
    annotation_grob <- grid::textGrob(
      annotation_text,
      x = ax_unit,
      y = ay_unit,
      rot = annotation_rot,
      gp = annotation_gp,
      name = aname
    )
  } else {
    annotation_grob <- grid::nullGrob(name = paste0(aname, ".null"))
  }

  # Center the label/quantity/annotation stack vertically on (ax, ay).
  # The bbox center then matches the anchor eunoia placed, so leader
  # endpoints (computed against an anchor-centered AABB) land on the
  # actual bbox edge instead of inside the stack.
  #
  # Each element's offset from `ay` is `0.5 * (below - above)`. We
  # only emit the terms that are actually present — adding a
  # `unit(0, "native")` to a y-position sum makes grid treat it as
  # an absolute position offset (not a zero height), which yanks the
  # resolution way off, so the zero-height branch must skip the term
  # entirely.
  build_above <- function(parts) {
    if (length(parts) == 0L) NULL else Reduce(`+`, parts)
  }
  if (has_label) {
    below_parts <- list()
    if (has_quantity) {
      below_parts <- c(below_parts, list(padding, grid::grobHeight(quantity_grob)))
    }
    if (has_annotation) {
      below_parts <- c(
        below_parts,
        list(padding, grid::grobHeight(annotation_grob))
      )
    }
    below <- build_above(below_parts)
    if (!is.null(below)) {
      label_grob$y <- ay_unit + 0.5 * below
    }
  }
  if (has_quantity) {
    above_parts <- list()
    below_parts <- list()
    if (has_label) {
      above_parts <- c(above_parts, list(grid::grobHeight(label_grob), padding))
    }
    if (has_annotation) {
      below_parts <- c(below_parts, list(padding, grid::grobHeight(annotation_grob)))
    }
    above <- build_above(above_parts)
    below <- build_above(below_parts)
    new_y <- ay_unit
    if (!is.null(below)) {
      new_y <- new_y + 0.5 * below
    }
    if (!is.null(above)) {
      new_y <- new_y - 0.5 * above
    }
    if (!is.null(above) || !is.null(below)) {
      quantity_grob$y <- new_y
    }
  }
  if (has_annotation) {
    above_parts <- list()
    if (has_quantity) {
      above_parts <- c(above_parts, list(grid::grobHeight(quantity_grob), padding))
    }
    if (has_label) {
      if (!has_quantity) {
        above_parts <- c(above_parts, list(padding))
      }
      above_parts <- c(above_parts, list(grid::grobHeight(label_grob)))
    }
    above <- build_above(above_parts)
    if (!is.null(above)) {
      annotation_grob$y <- ay_unit - 0.5 * above
    }
  }

  fallback_gp <- if (has_label) {
    label_gp
  } else if (has_quantity) {
    quantity_gp
  } else {
    annotation_gp
  }
  leader_grob <- build_leader_grob(
    ax = ax,
    ay = ay,
    kind = kind,
    tx = tx,
    ty = ty,
    lend_x = lend_x,
    lend_y = lend_y,
    waypoints_x = waypoints_x,
    waypoints_y = waypoints_y,
    leader_gp_list = leader_gp_list,
    fallback_gp = fallback_gp,
    name = leader_name
  )

  grid::gList(
    leader = leader_grob,
    label = label_grob,
    quantity = quantity_grob,
    annotation = annotation_grob
  )
}

#' Build the polyline leader for an exterior tag, or [grid::nullGrob()]
#' for interior / missing-tether placements.
#'
#' Draws the polyline `tether → waypoints[1..] → leader_end`. For
#' straight leaders (raycast / force-directed) `waypoints_*` are empty,
#' so the polyline collapses to the single `tether → leader_end`
#' segment. For elbow leaders eunoia emits one knee waypoint, producing
#' the orthogonal `tether → knee → leader_end` bend.
#'
#' Terminates at `(lend_x, lend_y)` — the point on the label box AABB
#' edge supplied by eunoia (`LabelPlacement::leader_end`). Falls back
#' to the anchor when the leader endpoint isn't finite so older /
#' partial placement results still draw something sensible.
#' @keywords internal
build_leader_grob <- function(
  ax,
  ay,
  kind,
  tx,
  ty,
  lend_x,
  lend_y,
  waypoints_x = numeric(0),
  waypoints_y = numeric(0),
  leader_gp_list,
  fallback_gp,
  name
) {
  is_exterior <- !is.null(kind) &&
    !is.na(kind) &&
    nzchar(kind) &&
    !identical(kind, "interior")
  if (!is_exterior) {
    return(grid::nullGrob(name = name))
  }
  if (
    is.null(tx) ||
      is.null(ty) ||
      !is.finite(tx) ||
      !is.finite(ty)
  ) {
    return(grid::nullGrob(name = name))
  }

  end_x <- if (!is.null(lend_x) && is.finite(lend_x)) lend_x else ax
  end_y <- if (!is.null(lend_y) && is.finite(lend_y)) lend_y else ay

  if (is.null(waypoints_x) || is.null(waypoints_y)) {
    waypoints_x <- numeric(0)
    waypoints_y <- numeric(0)
  }
  wp_ok <- is.finite(waypoints_x) & is.finite(waypoints_y)
  waypoints_x <- waypoints_x[wp_ok]
  waypoints_y <- waypoints_y[wp_ok]

  fallback_col <- if (
    !is.null(fallback_gp) &&
      !is.null(fallback_gp$col) &&
      length(fallback_gp$col) >= 1L
  ) {
    fallback_gp$col[1L]
  } else {
    "black"
  }
  pick <- function(field, default) {
    if (!is.null(leader_gp_list) && !is.null(leader_gp_list[[field]])) {
      leader_gp_list[[field]][1L]
    } else {
      default
    }
  }
  gp <- grid::gpar(
    col = pick("col", fallback_col),
    alpha = pick("alpha", 0.6),
    lwd = pick("lwd", 1),
    lty = pick("lty", 2),
    lex = pick("lex", 1)
  )

  grid::polylineGrob(
    x = c(tx, waypoints_x, end_x),
    y = c(ty, waypoints_y, end_y),
    default.units = "native",
    gp = gp,
    name = name
  )
}

#' Setup grobs for one tag (label + quantity + annotation + leader).
#'
#' Builds the gList via [build_tag_grobs()] and stashes the text / gpar
#' bundle on the resulting `EulerTag` gTree so [makeContent.EulerTags()]
#' can rebuild it at draw time with fresh measurements.
#'
#' @keywords internal
setup_tag <- function(data, labels, quantities, annotations, number) {
  has_label <- !is.null(labels) && !is.na(data$labels_par_id)
  has_quantity <- !is.null(quantities) && !is.na(data$quantities_par_id)
  has_annotation <- !is.null(annotations) &&
    !is.null(data$annotations_par_id) &&
    !is.na(data$annotations_par_id)

  label_text <- if (has_label) data$labels else NA
  quantity_text <- if (has_quantity) data$quantities else NA
  annotation_text <- if (has_annotation) data$annotations else NA
  label_gp <- if (has_label) labels$gp[data$labels_par_id] else NULL
  quantity_gp <- if (has_quantity) quantities$gp[data$quantities_par_id] else NULL
  annotation_gp <- if (has_annotation) {
    annotations$gp[data$annotations_par_id]
  } else {
    NULL
  }
  label_rot <- if (has_label) labels$rot[data$labels_par_id] else 0
  quantity_rot <- if (has_quantity) {
    quantities$rot[data$quantities_par_id]
  } else {
    0
  }
  annotation_rot <- if (has_annotation) {
    annotations$rot[data$annotations_par_id]
  } else {
    0
  }

  padding <- eulerr_options()$padding
  leader_gp_list <- if (!is.null(labels)) labels$leader else NULL

  wp <- data$leader_waypoints
  if (is.list(wp) && length(wp) >= 1L) {
    wp <- wp[[1L]]
  }
  wp_x <- if (!is.null(wp) && !is.null(wp$x)) wp$x else numeric(0)
  wp_y <- if (!is.null(wp) && !is.null(wp$y)) wp$y else numeric(0)

  grobs <- build_tag_grobs(
    ax = data$x,
    ay = data$y,
    kind = data$kind,
    tx = data$tether_x,
    ty = data$tether_y,
    lend_x = data$leader_end_x,
    lend_y = data$leader_end_y,
    label_text = label_text,
    quantity_text = quantity_text,
    annotation_text = annotation_text,
    has_label = has_label,
    has_quantity = has_quantity,
    has_annotation = has_annotation,
    label_gp = label_gp,
    quantity_gp = quantity_gp,
    annotation_gp = annotation_gp,
    label_rot = label_rot,
    quantity_rot = quantity_rot,
    annotation_rot = annotation_rot,
    number = number,
    leader_gp_list = leader_gp_list,
    padding = padding,
    waypoints_x = wp_x,
    waypoints_y = wp_y
  )

  grid::gTree(
    children = grobs,
    combo_key = rownames(data),
    label_text = label_text,
    quantity_text = quantity_text,
    annotation_text = annotation_text,
    has_label = has_label,
    has_quantity = has_quantity,
    has_annotation = has_annotation,
    label_gp = label_gp,
    quantity_gp = quantity_gp,
    annotation_gp = annotation_gp,
    label_rot = label_rot,
    quantity_rot = quantity_rot,
    annotation_rot = annotation_rot,
    leader_gp_list = leader_gp_list,
    padding = padding,
    number = number,
    name = paste("tag", "number", number, sep = "."),
    cl = "EulerTag"
  )
}

#' Setup the complement-count tag, in the same shape as a region tag so
#' it shares one [makeContent.EulerTags()] pass.
#'
#' Acts as a quantity-only tag with `combo_key = ""`. The complement
#' label text comes either from `complement$label` (user override) or
#' from `container_data$quantity_label` (the fitted complement count).
#'
#' @keywords internal
setup_complement_tag <- function(container_data, complement, number) {
  if (is.null(container_data)) {
    return(NULL)
  }
  label_text <- complement$label
  if (is.null(label_text)) {
    label_text <- container_data$quantity_label
  }
  if (
    is.null(label_text) ||
      is.na(label_text) ||
      !is.finite(container_data$label_x) ||
      !is.finite(container_data$label_y)
  ) {
    return(NULL)
  }
  cgp <- complement$gp
  quantity_gp <- grid::gpar(
    col = cgp$col[1L],
    alpha = cgp$alpha[1L],
    fontsize = cgp$fontsize[1L],
    cex = cgp$cex[1L],
    fontfamily = cgp$fontfamily[1L],
    lineheight = cgp$lineheight[1L],
    font = cgp$font[1L]
  )
  leader_gp_list <- complement$leader
  padding <- eulerr_options()$padding

  wp <- container_data$leader_waypoints
  wp_x <- if (!is.null(wp) && !is.null(wp$x)) wp$x else numeric(0)
  wp_y <- if (!is.null(wp) && !is.null(wp$y)) wp$y else numeric(0)

  grobs <- build_tag_grobs(
    ax = container_data$label_x,
    ay = container_data$label_y,
    kind = container_data$kind,
    tx = container_data$tether_x,
    ty = container_data$tether_y,
    lend_x = container_data$leader_end_x,
    lend_y = container_data$leader_end_y,
    label_text = NA,
    quantity_text = label_text,
    annotation_text = NA,
    has_label = FALSE,
    has_quantity = TRUE,
    has_annotation = FALSE,
    label_gp = NULL,
    quantity_gp = quantity_gp,
    annotation_gp = NULL,
    label_rot = 0,
    quantity_rot = 0,
    annotation_rot = 0,
    number = number,
    leader_gp_list = leader_gp_list,
    padding = padding,
    waypoints_x = wp_x,
    waypoints_y = wp_y,
    name_prefix = "complement"
  )

  grid::gTree(
    children = grobs,
    combo_key = "",
    label_text = NA,
    quantity_text = label_text,
    annotation_text = NA,
    has_label = FALSE,
    has_quantity = TRUE,
    has_annotation = FALSE,
    label_gp = NULL,
    quantity_gp = quantity_gp,
    annotation_gp = NULL,
    label_rot = 0,
    quantity_rot = 0,
    annotation_rot = 0,
    leader_gp_list = leader_gp_list,
    padding = padding,
    number = number,
    name_prefix = "complement",
    name = paste("tag", "number", number, "complement", sep = "."),
    cl = "EulerTag"
  )
}

#' Measure one tag's combined AABB in the current viewport's native units.
#' @keywords internal
measure_tag_native <- function(tag, padding_native) {
  label_w <- 0
  label_h <- 0
  if (isTRUE(tag$has_label)) {
    g <- grid::textGrob(tag$label_text, gp = tag$label_gp)
    label_w <- grid::convertWidth(
      grid::grobWidth(g),
      "native",
      valueOnly = TRUE
    )
    label_h <- grid::convertHeight(
      grid::grobHeight(g),
      "native",
      valueOnly = TRUE
    )
  }
  quant_w <- 0
  quant_h <- 0
  if (isTRUE(tag$has_quantity)) {
    g <- grid::textGrob(tag$quantity_text, gp = tag$quantity_gp)
    quant_w <- grid::convertWidth(
      grid::grobWidth(g),
      "native",
      valueOnly = TRUE
    )
    quant_h <- grid::convertHeight(
      grid::grobHeight(g),
      "native",
      valueOnly = TRUE
    )
  }
  annot_w <- 0
  annot_h <- 0
  if (isTRUE(tag$has_annotation)) {
    g <- grid::textGrob(tag$annotation_text, gp = tag$annotation_gp)
    annot_w <- grid::convertWidth(
      grid::grobWidth(g),
      "native",
      valueOnly = TRUE
    )
    annot_h <- grid::convertHeight(
      grid::grobHeight(g),
      "native",
      valueOnly = TRUE
    )
  }
  pad_lq <- if (label_h > 0 && quant_h > 0) padding_native else 0
  pad_qa <- if (quant_h > 0 && annot_h > 0) padding_native else 0
  pad_la <- if (quant_h == 0 && label_h > 0 && annot_h > 0) padding_native else 0
  list(
    w = max(label_w, quant_w, annot_w),
    h = label_h + quant_h + annot_h + pad_lq + pad_qa + pad_la
  )
}

#' Find the `EulerTags` child of an `EulerPanel`, if any.
#' @keywords internal
find_eulertags <- function(panel) {
  for (child in panel$children) {
    if (inherits(child, "EulerTags")) {
      return(child)
    }
  }
  NULL
}

#' Measure every drawable tag inside `tags_grob` against the current
#' viewport. Returns parallel vectors of combo / width / height suitable
#' for handing to [place_euler_labels()], plus the resolved leader gap
#' in native units (so the FFI sees one number per draw pass).
#' @keywords internal
measure_all_tags <- function(tags_grob, padding, gap = NULL) {
  if (is.null(tags_grob) || length(tags_grob$children) == 0L) {
    return(list(
      combos = character(),
      widths = numeric(),
      heights = numeric(),
      gap_native = 0
    ))
  }
  padding_native <- grid::convertHeight(
    padding,
    "native",
    valueOnly = TRUE
  )
  gap_native <- resolve_gap_native(gap, padding_native)
  combos <- character()
  widths <- numeric()
  heights <- numeric()
  for (child in tags_grob$children) {
    sz <- measure_tag_native(child, padding_native)
    if (sz$w > 0 && sz$h > 0 && is.finite(sz$w) && is.finite(sz$h)) {
      combos <- c(combos, if (is.null(child$combo_key)) "" else child$combo_key)
      widths <- c(widths, sz$w)
      heights <- c(heights, sz$h)
    }
  }
  list(
    combos = combos,
    widths = widths,
    heights = heights,
    gap_native = gap_native
  )
}

#' Fixed padding in pt for the panel viewport scale.
#'
#' Geometry / labels that land flush with the bbox would otherwise be
#' clipped at the device edge by stroke width and anti-aliasing — both
#' of which are in device units, not native units, so the padding is in
#' pt rather than a fraction of the coordinate range.
#' @keywords internal
EULER_PANEL_PAD_PT <- 3

#' Pad an axis range by `pt_pad` points, converted to native units
#' against a measurement viewport with the supplied scale. Returns the
#' original range unchanged if the conversion isn't finite (e.g. zero
#' range, no device).
#' @keywords internal
pad_axis_native <- function(
  lim,
  pt_pad,
  axis = c("x", "y"),
  layout_pos_row = NULL,
  layout_pos_col = NULL
) {
  axis <- match.arg(axis)
  if (!all(is.finite(lim)) || diff(range(lim)) <= 0) {
    return(lim)
  }
  meas_vp <- grid::viewport(
    layout.pos.row = layout_pos_row,
    layout.pos.col = layout_pos_col,
    xscale = lim,
    yscale = lim
  )
  grid::pushViewport(meas_vp)
  pad_native <- tryCatch(
    if (axis == "x") {
      grid::convertWidth(grid::unit(pt_pad, "pt"), "native", valueOnly = TRUE)
    } else {
      grid::convertHeight(grid::unit(pt_pad, "pt"), "native", valueOnly = TRUE)
    },
    error = function(e) NA_real_
  )
  grid::popViewport()
  if (!is.finite(pad_native) || pad_native <= 0) {
    return(lim)
  }
  c(lim[1] - pad_native, lim[2] + pad_native)
}

#' Set the panel viewport's `xscale`/`yscale` at draw time.
#'
#' Fires before grid pushes the panel viewport. We can therefore
#' measure labels against the live cell, run eunoia's placement, and
#' compute a viewport bbox that fits both the diagram and the labels.
#' On window resize grid invalidates the gTree and `makeContext`
#' re-runs, so the panel grows or shrinks to track the current device
#' and exterior labels never extend past the viewport edge.
#'
#' Aspect preservation: the new bbox keeps `xrng / yrng` equal to the
#' geometry's natural aspect (set by [setup_geometry()]) so that
#' circles render as circles. The smaller dimension is padded if the
#' label-driven canvas bbox is asymmetric.
#'
#' @export
#' @keywords internal
makeContext.EulerPanel <- function(x) {
  shapes <- x$shapes
  geom_xlim <- x$geom_xlim
  geom_ylim <- x$geom_ylim
  if (
    is.null(geom_xlim) ||
      is.null(geom_ylim) ||
      !all(is.finite(c(geom_xlim, geom_ylim)))
  ) {
    return(x)
  }

  layout_row <- if (is.null(x$vp)) NULL else x$vp$layout.pos.row
  layout_col <- if (is.null(x$vp)) NULL else x$vp$layout.pos.col

  # Always emit a vp so grid has something to push. We update its
  # xscale/yscale below; the layout.pos fields come from
  # `plot.euler.R`. The xscale/yscale gets a small pad in pt on each
  # side so geometry that lands flush with the bbox isn't clipped by
  # stroke width / anti-aliasing at the device edge.
  padded_geom_xlim <- pad_axis_native(
    geom_xlim,
    EULER_PANEL_PAD_PT,
    "x",
    layout_row,
    layout_col
  )
  padded_geom_ylim <- pad_axis_native(
    geom_ylim,
    EULER_PANEL_PAD_PT,
    "y",
    layout_row,
    layout_col
  )
  if (is.null(x$vp)) {
    x$vp <- grid::viewport(
      xscale = padded_geom_xlim,
      yscale = padded_geom_ylim,
      name = x$name %||% "panel.vp"
    )
  } else {
    x$vp$xscale <- padded_geom_xlim
    x$vp$yscale <- padded_geom_ylim
  }

  tags_grob <- find_eulertags(x)
  if (
    is.null(tags_grob) ||
      length(tags_grob$children) == 0L ||
      is.null(shapes) ||
      NROW(shapes) == 0L
  ) {
    return(x)
  }
  shape_type <- shapes$type[1L]

  geom_xrng <- diff(geom_xlim)
  geom_yrng <- diff(geom_ylim)
  if (geom_xrng <= 0 || geom_yrng <= 0) {
    return(x)
  }
  geom_ar <- geom_xrng / geom_yrng

  # The cell's pt aspect is set by `canvas_vp`'s `grid.layout`. We must
  # use that aspect for the new xlim/ylim so circles render as circles
  # AND the panel viewport's pt extent matches what the cell can give
  # us — otherwise exterior label anchors at the edge of `xscale`/
  # `yscale` may map past the cell and clip at the device edge.
  cell_vp <- grid::viewport(
    layout.pos.row = x$vp$layout.pos.row,
    layout.pos.col = x$vp$layout.pos.col
  )
  grid::pushViewport(cell_vp)
  cell_w_pt <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  cell_h_pt <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  grid::popViewport()
  if (
    !is.finite(cell_w_pt) ||
      !is.finite(cell_h_pt) ||
      cell_w_pt <= 0 ||
      cell_h_pt <= 0
  ) {
    cell_ar <- geom_ar
  } else {
    cell_ar <- cell_w_pt / cell_h_pt
  }

  container <- x$container
  has_container <- !is.null(container)
  placement_opts <- resolve_placement_opts(x$placement_opts)
  precision <- if (
    is.null(x$label_precision) || !is.finite(x$label_precision)
  ) {
    max(geom_xrng, geom_yrng) / 100
  } else {
    x$label_precision
  }

  # Fixed-point loop. Each iteration measures labels at the current
  # candidate xlim/ylim, runs placement, and grows the viewport to
  # include `canvas_bbox`. Label native size scales with `xrng/yrng`
  # (font is in pt; bigger native extent => bigger native label), so
  # growing the viewport between iterations grows the next iteration's
  # labels. The loop converges as long as the labels fit inside the
  # cell at all; we cap at a small iteration count to keep extreme
  # corner cases from spinning. Whether converged or not, the LAST
  # iteration's xlim/ylim is what we pin on the viewport — that's also
  # what `makeContent.EulerTags` will re-measure against, so panel
  # bbox and label positions stay consistent.
  # Cap each axis at 10x the geometry extent. When the label is wider
  # than the cell (label_pt > cell_pt), the fixed-point iteration has
  # no solution and the bbox would otherwise diverge. Clamping at a
  # finite multiple gives a coherent (if zoomed-out) frame — the
  # diagram still draws and the user can see what's happening rather
  # than getting a runaway bbox.
  max_iters <- 30L
  cap_factor <- 20
  cap_xrng <- geom_xrng * cap_factor
  cap_yrng <- geom_yrng * cap_factor
  # Seed the iteration with the same pt-based pad we'd otherwise apply
  # post-loop, so each measurement viewport matches what
  # `makeContent.EulerTags` will measure against at draw time. Without
  # this, post-loop padding inflates the native viewport by ~1-2% and
  # `makeContent` can re-measure labels just large enough that some flip
  # from interior to exterior — but the viewport is already pinned, so
  # the exterior labels overflow the panel and get clipped.
  current_xlim <- pad_axis_native(
    geom_xlim,
    EULER_PANEL_PAD_PT,
    "x",
    layout_row,
    layout_col
  )
  current_ylim <- pad_axis_native(
    geom_ylim,
    EULER_PANEL_PAD_PT,
    "y",
    layout_row,
    layout_col
  )
  for (iter in seq_len(max_iters)) {
    meas_vp <- grid::viewport(
      layout.pos.row = x$vp$layout.pos.row,
      layout.pos.col = x$vp$layout.pos.col,
      xscale = current_xlim,
      yscale = current_ylim
    )
    grid::pushViewport(meas_vp)
    measurements <- tryCatch(
      measure_all_tags(tags_grob, x$padding, placement_opts$gap),
      error = function(e) NULL
    )
    grid::popViewport()
    if (is.null(measurements) || length(measurements$combos) == 0L) {
      break
    }

    placements <- tryCatch(
      place_euler_labels(
        set_names = rownames(shapes),
        shape = shape_type,
        h = shapes$h,
        k = shapes$k,
        a = shapes$a,
        b = shapes$b,
        phi = shapes$phi,
        width = shapes$width,
        height = shapes$height,
        side = shapes$side,
        container_h = if (has_container) container$h else NULL,
        container_k = if (has_container) container$k else NULL,
        container_width = if (has_container) container$width else NULL,
        container_height = if (has_container) container$height else NULL,
        n_vertices = as.integer(x$n_vertices),
        label_combos = measurements$combos,
        label_widths = measurements$widths,
        label_heights = measurements$heights,
        placement = placement_opts$placement,
        placement_margin = placement_opts$margin,
        placement_iterations = placement_opts$iterations,
        placement_min_gap = placement_opts$min_gap,
        placement_tether = placement_opts$tether,
        placement_leader_gap = measurements$gap_native,
        label_precision = precision
      ),
      error = function(e) NULL
    )
    if (
      is.null(placements) ||
        !is.finite(placements$canvas_bbox_h) ||
        !is.finite(placements$canvas_bbox_width) ||
        placements$canvas_bbox_width <= 0 ||
        placements$canvas_bbox_height <= 0
    ) {
      break
    }

    cb_xmin <- placements$canvas_bbox_h -
      placements$canvas_bbox_width / 2
    cb_xmax <- placements$canvas_bbox_h +
      placements$canvas_bbox_width / 2
    cb_ymin <- placements$canvas_bbox_k -
      placements$canvas_bbox_height / 2
    cb_ymax <- placements$canvas_bbox_k +
      placements$canvas_bbox_height / 2

    xmin <- min(geom_xlim[1], cb_xmin)
    xmax <- max(geom_xlim[2], cb_xmax)
    ymin <- min(geom_ylim[1], cb_ymin)
    ymax <- max(geom_ylim[2], cb_ymax)
    xrng <- xmax - xmin
    yrng <- ymax - ymin
    cx <- (xmin + xmax) / 2
    cy <- (ymin + ymax) / 2
    if (xrng / yrng > cell_ar) {
      yrng <- xrng / cell_ar
    } else {
      xrng <- yrng * cell_ar
    }
    new_xlim <- c(cx - xrng / 2, cx + xrng / 2)
    new_ylim <- c(cy - yrng / 2, cy + yrng / 2)

    new_xrng <- diff(new_xlim)
    new_yrng <- diff(new_ylim)
    rel_change <- max(
      abs(new_xrng - diff(current_xlim)) / max(diff(current_xlim), 1e-9),
      abs(new_yrng - diff(current_ylim)) / max(diff(current_ylim), 1e-9)
    )

    # Clamp at cap. Once we hit the cap, stop iterating.
    capped <- FALSE
    if (new_xrng > cap_xrng) {
      mid <- (new_xlim[1] + new_xlim[2]) / 2
      new_xlim <- c(mid - cap_xrng / 2, mid + cap_xrng / 2)
      capped <- TRUE
    }
    if (new_yrng > cap_yrng) {
      mid <- (new_ylim[1] + new_ylim[2]) / 2
      new_ylim <- c(mid - cap_yrng / 2, mid + cap_yrng / 2)
      capped <- TRUE
    }
    current_xlim <- new_xlim
    current_ylim <- new_ylim
    if (rel_change < 0.005 || capped) {
      break
    }
  }

  x$vp$xscale <- current_xlim
  x$vp$yscale <- current_ylim
  x
}

#' Re-place tags at draw time so resizing the device doesn't clip labels.
#'
#' The panel viewport is active when `makeContent` fires, so we can
#' measure each tag's footprint in current native units via
#' `grid::convertWidth(grobWidth(...), "native")` and call eunoia's
#' [place_euler_labels()] again. On resize, grid invalidates the tree
#' and `makeContent` re-runs, so the placement tracks the current
#' device automatically.
#'
#' Tags whose measured size is zero (or whose anchor / kind eunoia
#' can't compute) keep the positions they were built with — typically
#' the setup-time placement stored on `centers$x` / `centers$y`.
#'
#' @export
#' @keywords internal
makeContent.EulerTags <- function(x) {
  n <- length(x$children)
  if (n == 0L) {
    return(x)
  }
  shapes <- x$shapes
  if (is.null(shapes) || NROW(shapes) == 0L) {
    return(x)
  }
  shape_type <- shapes$type[1L]

  padding_native <- grid::convertHeight(
    x$padding,
    "native",
    valueOnly = TRUE
  )

  combos <- character(n)
  widths <- numeric(n)
  heights <- numeric(n)
  for (i in seq_len(n)) {
    child <- x$children[[i]]
    combos[i] <- if (is.null(child$combo_key)) "" else child$combo_key
    sz <- measure_tag_native(child, padding_native)
    widths[i] <- sz$w
    heights[i] <- sz$h
  }

  ok <- widths > 0 & heights > 0 & is.finite(widths) & is.finite(heights)
  if (!any(ok)) {
    return(x)
  }

  container <- x$container
  has_container <- !is.null(container)
  placement_opts <- resolve_placement_opts(x$placement_opts)
  gap_native <- resolve_gap_native(placement_opts$gap, padding_native)

  placements <- place_euler_labels(
    set_names = rownames(shapes),
    shape = shape_type,
    h = shapes$h,
    k = shapes$k,
    a = shapes$a,
    b = shapes$b,
    phi = shapes$phi,
    width = shapes$width,
    height = shapes$height,
    side = shapes$side,
    container_h = if (has_container) container$h else NULL,
    container_k = if (has_container) container$k else NULL,
    container_width = if (has_container) container$width else NULL,
    container_height = if (has_container) container$height else NULL,
    n_vertices = as.integer(x$n_vertices),
    label_combos = combos[ok],
    label_widths = widths[ok],
    label_heights = heights[ok],
    placement = placement_opts$placement,
    placement_margin = placement_opts$margin,
    placement_iterations = placement_opts$iterations,
    placement_min_gap = placement_opts$min_gap,
    placement_tether = placement_opts$tether,
    placement_leader_gap = gap_native,
    label_precision = x$label_precision
  )

  waypoints <- split_waypoints(placements)

  ok_idx <- which(ok)
  for (j in seq_along(ok_idx)) {
    i <- ok_idx[j]
    ax <- placements$anchor_x[j]
    ay <- placements$anchor_y[j]
    if (!is.finite(ax) || !is.finite(ay)) {
      next
    }
    kind <- placements$kind[j]
    tx <- placements$tether_x[j]
    ty <- placements$tether_y[j]
    lend_x <- placements$leader_end_x[j]
    lend_y <- placements$leader_end_y[j]
    wp <- if (!is.null(waypoints)) {
      waypoints[[j]]
    } else {
      list(x = numeric(0), y = numeric(0))
    }
    child <- x$children[[i]]
    new_children <- build_tag_grobs(
      ax = ax,
      ay = ay,
      kind = kind,
      tx = tx,
      ty = ty,
      lend_x = lend_x,
      lend_y = lend_y,
      label_text = child$label_text,
      quantity_text = child$quantity_text,
      annotation_text = child$annotation_text,
      has_label = isTRUE(child$has_label),
      has_quantity = isTRUE(child$has_quantity),
      has_annotation = isTRUE(child$has_annotation),
      label_gp = child$label_gp,
      quantity_gp = child$quantity_gp,
      annotation_gp = child$annotation_gp,
      label_rot = child$label_rot,
      quantity_rot = child$quantity_rot,
      annotation_rot = child$annotation_rot,
      number = child$number,
      leader_gp_list = child$leader_gp_list,
      padding = child$padding,
      waypoints_x = wp$x,
      waypoints_y = wp$y,
      name_prefix = if (is.null(child$name_prefix)) "tag" else child$name_prefix
    )
    # `grid::setChildren()` updates the internal `childrenOrder` index
    # alongside `$children`; direct `$children <-` assignment leaves
    # the order stale and grid silently skips drawing the new grobs.
    x$children[[i]] <- grid::setChildren(child, new_children)
  }

  x
}
