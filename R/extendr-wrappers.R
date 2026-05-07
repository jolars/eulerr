#' Fit an Euler diagram using the eunoia Rust library.

# nolint start

#' @keywords internal
fit_euler_diagram <- function(
  combo_names,
  combo_values,
  input,
  shape,
  loss,
  extraopt_threshold,
  tolerance,
  max_sets,
  seed
) {
  .Call(
    wrap__fit_euler_diagram,
    combo_names,
    combo_values,
    input,
    shape,
    loss,
    extraopt_threshold,
    tolerance,
    max_sets,
    seed
  )
}

#' Compute polygon geometry and label anchors for plotting a fitted Euler
#' diagram.
#'
#' Inputs are the fitted shape parameters for the **non-empty** sets only,
#' in the order eulerr stores them (`x$ellipses` rows after dropping rows
#' with NA). The output is a sparse, parallel set of vectors keyed by
#' region label (set names joined by `&` in input order). Lengths equal the
#' number of populated regions returned by `decompose_regions`.
#'
#' @keywords internal
euler_plot_data <- function(
  set_names,
  h,
  k,
  a,
  b,
  phi,
  n_vertices,
  label_precision
) {
  .Call(
    wrap__euler_plot_data,
    set_names,
    h,
    k,
    a,
    b,
    phi,
    n_vertices,
    label_precision
  )
}

#' Clip a (possibly multi-polygon) subject path against a single clip
#' polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr
#' actually uses at the stripe-pattern site.
#'
#' @keywords internal
polygon_clip_rust <- function(
  subject_x,
  subject_y,
  subject_id_lengths,
  clip_x,
  clip_y,
  op
) {
  .Call(
    wrap__polygon_clip_rust,
    subject_x,
    subject_y,
    subject_id_lengths,
    clip_x,
    clip_y,
    op
  )
}

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
