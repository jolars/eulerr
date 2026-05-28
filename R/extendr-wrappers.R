#' Fit an Euler diagram using the eunoia Rust library.

# nolint start

#' @keywords internal
fit_euler_diagram <- function(combo_names, combo_values, input, shape, loss, extraopt_threshold, tolerance, max_sets, complement, seed) .Call(wrap__fit_euler_diagram, combo_names, combo_values, input, shape, loss, extraopt_threshold, tolerance, max_sets, complement, seed)

#' Compute polygon geometry and label anchors for plotting a fitted Euler
#' diagram, including the optional complement region inside a fitted
#' container.
#'
#' Inputs are the fitted shape parameters for the **non-empty** sets only,
#' in the order eulerr stores them (`x$shapes` rows after dropping rows
#' with NA). The leading `shape` argument selects which per-set columns are
#' active: `"circle"`/`"ellipse"` consume `(h, k, a, b, phi)`,
#' `"rectangle"` consumes `(h, k, width, height)`, `"square"` consumes
#' `(h, k, side)`. The remaining vectors must still be provided (NaN
#' padding is acceptable). When `container_*` are non-NULL they describe
#' the fitted universe-box rectangle; in that case the result also carries
#' the complement region geometry (the area inside the rectangle outside
#' every shape) and a label anchor for it. Eunoia's `decompose_regions`
#' emits this region under the empty `Combination` whenever the spec
#' carries a complement and a container is supplied.
#'
#' @keywords internal
euler_plot_data <- function(set_names, shape, h, k, a, b, phi, width, height, side, container_h, container_k, container_width, container_height, n_vertices, label_precision) .Call(wrap__euler_plot_data, set_names, shape, h, k, a, b, phi, width, height, side, container_h, container_k, container_width, container_height, n_vertices, label_precision)

#' Place per-region labels using eunoia's `place_labels` API.
#'
#' Inputs mirror `euler_plot_data` for shape geometry and add per-region
#' label sizes plus placement-strategy options. Returns, parallel to
#' `label_combos`:
#'
#' * `anchor_x` / `anchor_y` — placed label anchor (NA on miss);
#' * `kind` — one of `"interior"`, `"exterior_raycast"`,
#'   `"exterior_force_directed"`, `"exterior_elbow"`; `""` if no
#'   placement was produced;
#' * `tether_x` / `tether_y` — tether point for the leader line (NA for
#'   interior placements / misses).
#' * `leader_end_x` / `leader_end_y` — point on the label box AABB where
#'   the leader terminates (NA for interior placements / misses).
#' * `leader_waypoints_x` / `leader_waypoints_y` / `leader_waypoints_lengths`
#'   — concatenated waypoint coordinates and per-label counts. Empty for
#'   straight leaders; carries one knee point per elbow placement.
#'
#' Plus a canvas bbox (`canvas_bbox_h/k/width/height`) from eunoia's
#' `placements_bbox` — NaN when no placements were produced — for the R
#' side to grow `xlim/ylim` so exterior labels are never clipped.
#'
#' The complement region is requested with `""` in `label_combos`; when
#' `container_*` are non-NULL the spec is built with `.complement(1.0)`
#' so eunoia emits the empty `Combination` from `decompose_regions`.
#'
#' @keywords internal
place_euler_labels <- function(set_names, shape, h, k, a, b, phi, width, height, side, container_h, container_k, container_width, container_height, n_vertices, label_combos, label_widths, label_heights, placement, placement_margin, placement_iterations, placement_min_gap, placement_tether, placement_leader_gap, label_precision) .Call(wrap__place_euler_labels, set_names, shape, h, k, a, b, phi, width, height, side, container_h, container_k, container_width, container_height, n_vertices, label_combos, label_widths, label_heights, placement, placement_margin, placement_iterations, placement_min_gap, placement_tether, placement_leader_gap, label_precision)

#' Clip a (possibly multi-polygon) subject path against a single clip
#' polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr
#' actually uses at the stripe-pattern site.
#'
#' @keywords internal
polygon_clip_rust <- function(subject_x, subject_y, subject_id_lengths, clip_x, clip_y, op) .Call(wrap__polygon_clip_rust, subject_x, subject_y, subject_id_lengths, clip_x, clip_y, op)

#' Default number of sets that `eunoia` accepts before rejecting a spec.
#' Used by the R-side input validator so the cap is not hardcoded.
#' @keywords internal
max_sets_default <- function() .Call(wrap__max_sets_default)

#' Absolute upper bound on the number of sets that `eunoia` can represent
#' in a single diagram. Used by the R-side input validator so the cap is
#' not hardcoded.
#' @keywords internal
max_sets_hard_cap <- function() .Call(wrap__max_sets_hard_cap)


# nolint end
