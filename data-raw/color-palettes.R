# Generate a set of qualitative color palettes using qualpalr -------------

palettes <- vector("list", 20)

# Optimizer wrapper -------------------------------------------------------

optimize_qualpal <- function(x, target, n) {
  fit <- qualpal(n, cvd = "deutan", colorspace = "pretty", cvd_severity = x)

  # Cost function
  (fit$min_de_DIN99d - target) ^ 2
}

# Try to optimize color palettes to maximum cvd_severity adaptation -------

for (i in seq_along(palettes)) {
  if (i == 1) next
  fit <- stats::optimize(optimize_qualpal, target = 18, n = i, lower = 0,
                         upper  = 1)

  palettes[[i]] <- qualpalr::qualpal(i, cvd = "deutan", colorspace = "pretty",
                                     cvd_severity = fit$minimum)$hex
}

# Save as a data file to be used inside eulerr ----------------------------

devtools::use_data(
  palettes,
  internal = TRUE,
  compress = "gzip",
  overwrite = TRUE
)
