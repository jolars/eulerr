#' Bit-index matrix: (2^n - 1) × n integer matrix where row i's column j

# nolint start

#' indicates whether set j is included in subset i. Rows are in legacy
#' cardinality-first order (see `legacy_subset_masks`) so that downstream
#' positional code (`fills.grob.<i>`, plotter region indexing) keeps
#' matching the legacy Rcpp/C++ behavior.
#' @keywords internal
bit_index_rust <- function(n) .Call(wrap__bit_index_rust, n)

#' Fit an Euler diagram using the eunoia Rust library.
#' @keywords internal
fit_euler_diagram <- function(combo_names, combo_values, input, shape, loss, loss_aggregator, extraopt_threshold, tolerance, seed) .Call(wrap__fit_euler_diagram, combo_names, combo_values, input, shape, loss, loss_aggregator, extraopt_threshold, tolerance, seed)

#' Compute polygon geometry and label anchors for plotting a fitted Euler
#' diagram.
#'
#' Inputs are the fitted shape parameters for the **non-empty** sets only,
#' in the order eulerr stores them (`x$ellipses` rows after dropping rows
#' with NA). The output is positional over `2^n - 1` rows in eulerr's
#' `bit_indexr(n)` order, with `NULL` entries for regions the fitted
#' geometry doesn't populate.
#'
#' @keywords internal
euler_plot_data <- function(set_names, h, k, a, b, phi, n_vertices, label_precision) .Call(wrap__euler_plot_data, set_names, h, k, a, b, phi, n_vertices, label_precision)

#' Clip a (possibly multi-polygon) subject path against a single clip
#' polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr
#' actually uses at the stripe-pattern site.
#'
#' @keywords internal
polygon_clip_rust <- function(subject_x, subject_y, subject_id_lengths, clip_x, clip_y, op) .Call(wrap__polygon_clip_rust, subject_x, subject_y, subject_id_lengths, clip_x, clip_y, op)


# nolint end
