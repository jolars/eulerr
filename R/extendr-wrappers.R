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


# nolint end
