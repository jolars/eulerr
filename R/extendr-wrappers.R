#' Fit an Euler diagram using the eunoia Rust library.

# nolint start

#' @keywords internal
fit_euler_diagram <- function(combo_names, combo_values, input, shape, loss, extraopt_threshold, tolerance, max_sets, complement, seed) .Call(wrap__fit_euler_diagram, combo_names, combo_values, input, shape, loss, extraopt_threshold, tolerance, max_sets, complement, seed)

#' Compute polygon geometry and label anchors for plotting a fitted Euler
#' diagram, including the optional complement region inside a fitted
#' container.
#'
#' Inputs are the fitted shape parameters for the **non-empty** sets only,
#' in the order eulerr stores them (`x$ellipses` rows after dropping rows
#' with NA). When `container_*` are non-NULL they describe the fitted
#' universe-box rectangle; in that case the result also carries the
#' complement region geometry (the area inside the rectangle outside every
#' shape) and a label anchor for it. Eunoia's `decompose_regions` emits this
#' region under the empty `Combination` whenever the spec carries a
#' complement and a container is supplied — so eulerr no longer needs a
#' hand-rolled rectangle-minus-shapes pass.
#'
#' @keywords internal
euler_plot_data <- function(set_names, h, k, a, b, phi, container_h, container_k, container_width, container_height, n_vertices, label_precision) .Call(wrap__euler_plot_data, set_names, h, k, a, b, phi, container_h, container_k, container_width, container_height, n_vertices, label_precision)

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
