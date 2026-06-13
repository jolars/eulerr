# Benchmark eulerr against competing CRAN/Bioconductor area-proportional
# Euler/Venn fitters. This script is *not* part of the package build (the
# `data-raw/` directory is listed in `.Rbuildignore`). It is run manually to
# produce `vignettes/benchmark_results.rds`, which the `comparison.Rmd` vignette
# reads and displays. Keeping the heavy, dependency-laden computation here means
# the vignette itself builds on CRAN with only eulerr + knitr + lattice.
#
# To run it you need the competitor packages, which cannot be CRAN build
# dependencies (venneuler needs rJava, nVennR is GitHub-only):
#
#   install.packages(c("venneuler", "BioVenn", "bench"))
#   remotes::install_github("vqf/nVennR")
#
# Then, from the package root:
#
#   Rscript data-raw/benchmarks.R
#
# Any competitor that fails to load is skipped (with a message) rather than
# aborting the run, so partial results are still produced. The set of packages
# that actually ran is recorded in the `meta` slot of the output.

library(eulerr)

set.seed(20240611) # only affects synthetic ID ordering for BioVenn/nVennR

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Common accuracy metric
#
# Every package is scored with eulerr's own goodness-of-fit definitions, applied
# uniformly to the *realized* geometry each package produces. We convert fitted
# shapes (circles/ellipses) into dense polygons, compute the area of every
# disjoint region with polyclip, and then evaluate stress / diagError exactly as
# eulerr does internally (see R/euler.R and R/fit_diagram.R). This is the only
# way to compare packages fairly: each is judged on the same metric, computed
# the same way, regardless of what it optimizes internally.
# ---------------------------------------------------------------------------

# Signed area of a single polygon ring (shoelace formula).
shoelace <- function(x, y) {
  n <- length(x)
  j <- c(n, seq_len(n - 1L))
  0.5 * sum(x[j] * y - x * y[j])
}

# Net area of a polyclip result (a list of rings; holes carry opposite
# orientation, so summing signed areas nets them out).
poly_area <- function(p) {
  if (length(p) == 0L) {
    return(0)
  }
  abs(sum(vapply(p, function(r) shoelace(r$x, r$y), numeric(1))))
}

# Parameterize an ellipse (or circle, with a == b, phi == 0) as a polygon.
ellipse_polygon <- function(h, k, a, b, phi = 0, n = 200L) {
  t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
  ct <- cos(t)
  st <- sin(t)
  list(
    x = h + a * ct * cos(phi) - b * st * sin(phi),
    y = k + a * ct * sin(phi) + b * st * cos(phi)
  )
}

# Given a named list of set polygons (each `list(x, y)`), return the area of
# every non-empty disjoint region, keyed by eulerr-style "A&B" labels.
region_areas <- function(set_polys) {
  set_names <- names(set_polys)
  n <- length(set_names)
  ids <- seq_len(n)

  out <- numeric(0)
  for (k in seq_len(2L^n - 1L)) {
    in_set <- as.logical(intToBits(k))[ids]
    inside <- ids[in_set]
    outside <- ids[!in_set]

    # Intersection of all sets that should be inside this region.
    cur <- list(set_polys[[inside[1L]]])
    for (i in inside[-1L]) {
      cur <- polyclip::polyclip(cur, list(set_polys[[i]]), op = "intersection")
      if (length(cur) == 0L) break
    }
    # Subtract every set that should be outside this region.
    if (length(cur) > 0L) {
      for (j in outside) {
        cur <- polyclip::polyclip(cur, list(set_polys[[j]]), op = "minus")
        if (length(cur) == 0L) break
      }
    }

    label <- paste(set_names[inside], collapse = "&")
    out[label] <- poly_area(cur)
  }
  out
}

# Set names referenced by a disjoint combination vector, in first-seen order.
set_names_of <- function(combos) {
  unique(unlist(strsplit(names(combos), "&", fixed = TRUE)))
}

# Expand a disjoint combination vector to *all* 2^n - 1 regions, filling
# unspecified regions with 0 (these are the regions eulerr also penalizes when
# they are drawn with non-zero area). Returns a named numeric vector.
expand_combos <- function(combos) {
  sets <- set_names_of(combos)
  n <- length(sets)
  labels <- character(0)
  for (k in seq_len(2L^n - 1L)) {
    in_set <- as.logical(intToBits(k))[seq_len(n)]
    labels <- c(labels, paste(sets[in_set], collapse = "&"))
  }
  orig <- setNames(numeric(length(labels)), labels)
  orig[names(combos)] <- combos
  orig
}

# eulerr's stress and diagError, computed from original vs realized areas.
# `orig` and `fitted` are named region vectors over the same label universe.
fit_metrics <- function(orig, fitted) {
  labs <- names(orig)
  f <- fitted[labs]
  f[is.na(f)] <- 0
  o <- as.numeric(orig)
  f <- as.numeric(f)

  region_error <- abs(o / sum(o) - f / sum(f))
  diag_error <- max(region_error)
  beta <- sum(f * o) / sum(o^2)
  stress <- sum((f - beta * o)^2) / sum(f^2)

  list(
    stress = stress,
    diagError = diag_error,
    regionError = setNames(region_error, labs)
  )
}

# Convenience: score an arbitrary set of circles against a combination vector.
score_circles <- function(combos, x, y, r, set_names) {
  polys <- setNames(
    Map(function(xi, yi, ri) ellipse_polygon(xi, yi, ri, ri), x, y, r),
    set_names
  )
  fit_metrics(expand_combos(combos), region_areas(polys))
}

# ---------------------------------------------------------------------------
# Timing helper: median wall-clock per fit, in milliseconds.
# ---------------------------------------------------------------------------

time_ms <- function(expr, reps = 5L) {
  expr <- substitute(expr)
  env <- parent.frame()
  if (requireNamespace("bench", quietly = TRUE)) {
    res <- bench::mark(
      eval(expr, env),
      iterations = reps,
      check = FALSE,
      filter_gc = FALSE
    )
    return(as.numeric(res$median) * 1000)
  }
  times <- replicate(reps, system.time(eval(expr, env))[["elapsed"]])
  stats::median(times) * 1000
}

# ---------------------------------------------------------------------------
# Synthetic ID lists from a disjoint combination vector, for packages
# (BioVenn, nVennR) that take element lists rather than counts.
# ---------------------------------------------------------------------------

combos_to_lists <- function(combos) {
  sets <- set_names_of(combos)
  lists <- setNames(vector("list", length(sets)), sets)
  counter <- 0L
  for (label in names(combos)) {
    n_el <- round(combos[[label]])
    if (n_el <= 0L) next
    ids <- counter + seq_len(n_el)
    counter <- counter + n_el
    members <- strsplit(label, "&", fixed = TRUE)[[1L]]
    for (s in members) {
      lists[[s]] <- c(lists[[s]], ids)
    }
  }
  lists
}

# ---------------------------------------------------------------------------
# Per-package fit wrappers. Each returns a list with `metrics` (from
# fit_metrics) and `time_ms`, or NULL if the package is unavailable / the input
# is out of its supported range. Geometry is always scored with the common
# metric above.
# ---------------------------------------------------------------------------

fit_eulerr <- function(combos, shape = "circle", loss = "stress", reps = 5L) {
  fit <- euler(combos, shape = shape, loss = loss)
  t <- time_ms(euler(combos, shape = shape, loss = loss), reps)

  # eulerr exposes the metrics directly; recompute from geometry too and warn on
  # large disagreement, which would mean the common-metric helper has drifted
  # from eulerr's internal accounting.
  shapes <- fit$shapes
  keep <- !is.na(shapes$h)
  polys <- setNames(
    lapply(which(keep), function(i) {
      ellipse_polygon(
        shapes$h[i],
        shapes$k[i],
        shapes$a[i],
        shapes$b[i],
        shapes$phi[i]
      )
    }),
    rownames(shapes)[keep]
  )
  m <- fit_metrics(expand_combos(combos), region_areas(polys))
  if (abs(m$stress - fit$stress) > 0.02) {
    warning(
      "common-metric stress (",
      round(m$stress, 4),
      ") disagrees with eulerr$stress (",
      round(fit$stress, 4),
      ") for a fit; check region_areas()."
    )
  }

  # Report eulerr's own (authoritative) numbers.
  list(
    metrics = list(
      stress = fit$stress,
      diagError = fit$diagError,
      regionError = fit$regionError
    ),
    time_ms = t
  )
}

fit_venneuler <- function(combos, reps = 5L) {
  if (!requireNamespace("venneuler", quietly = TRUE)) {
    return(NULL)
  }
  v <- venneuler::venneuler(combos)
  t <- time_ms(venneuler::venneuler(combos), reps)
  m <- score_circles(
    combos,
    x = v$centers[, 1L],
    y = v$centers[, 2L],
    r = v$diameters / 2,
    set_names = v$labels
  )
  list(metrics = m, time_ms = t)
}

fit_biovenn <- function(combos, reps = 5L) {
  if (!requireNamespace("BioVenn", quietly = TRUE)) {
    return(NULL)
  }
  sets <- set_names_of(combos)
  if (length(sets) < 2L || length(sets) > 3L) {
    return(NULL) # BioVenn handles only 2 or 3 circles
  }
  lists <- combos_to_lists(combos)
  args <- list(
    list_x = lists[[1L]],
    list_y = lists[[2L]],
    list_z = if (length(sets) == 3L) lists[[3L]] else NULL,
    title = "",
    subtitle = "",
    output = "screen", # avoids writing a file
    nrtype = "abs"
  )
  # draw.venn renders a plot; silence it.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  bv <- do.call(BioVenn::draw.venn, args)
  t <- time_ms(
    {
      grDevices::pdf(NULL)
      do.call(BioVenn::draw.venn, args)
      grDevices::dev.off()
    },
    reps
  )

  # BioVenn returns circle centers and radii. Field names are resolved in
  # `biovenn_geometry()`; if your installed version differs, inspect `str(bv)`.
  geom <- biovenn_geometry(bv, sets)
  if (is.null(geom)) {
    return(NULL)
  }
  m <- score_circles(combos, geom$x, geom$y, geom$r, sets)
  list(metrics = m, time_ms = t)
}

# Extract circle centers/radii from a BioVenn result across its 2- and 3-set
# layouts. BioVenn names its outputs x/y/z; coordinates live in `*_c` (centre)
# and radii in `*_r`. Returns NULL if the expected fields are absent.
biovenn_geometry <- function(bv, sets) {
  cx <- c(bv$x_c, bv$y_c, bv$z_c)
  cy <- c(bv$x_c_y %||% NA, bv$y_c_y %||% NA, bv$z_c_y %||% NA)
  r <- c(bv$x_r, bv$y_r, bv$z_r)
  # Some BioVenn versions return a single data frame `bv$coordinates`; prefer it.
  if (!is.null(bv$coordinates)) {
    co <- bv$coordinates
    return(list(
      x = co$x[seq_along(sets)],
      y = co$y[seq_along(sets)],
      r = co$radius[seq_along(sets)]
    ))
  }
  if (anyNA(c(cx, r)) || length(r) < length(sets)) {
    return(NULL)
  }
  list(x = cx[seq_along(sets)], y = cy[seq_along(sets)], r = r[seq_along(sets)])
}

fit_nvennr <- function(combos, reps = 3L) {
  if (!requireNamespace("nVennR", quietly = TRUE)) {
    return(NULL)
  }
  lists <- combos_to_lists(combos)
  obj <- nVennR::plotVenn(lists, showPlot = FALSE, setColors = NULL)
  t <- time_ms(nVennR::plotVenn(lists, showPlot = FALSE), reps)

  areas <- nvennr_region_areas(obj, combos)
  if (is.null(areas)) {
    return(NULL)
  }
  m <- fit_metrics(expand_combos(combos), areas)
  list(metrics = m, time_ms = t)
}

# nVennR draws irregular polygons; recover the realized area of each region.
# The nVennObj stores region polygons; the exact accessor varies by version, so
# this is best-effort. If the structure is not as expected it returns NULL and
# the comparison for nVennR is skipped (logged in `meta`).
nvennr_region_areas <- function(obj, combos) {
  reg <- tryCatch(obj$reg, error = function(e) NULL)
  if (is.null(reg)) {
    return(NULL)
  }
  # `obj$reg` keys regions by a binary membership string over sets in `obj$set`
  # order; each entry should carry polygon vertices. Map those to "A&B" labels.
  sets <- obj$set
  out <- numeric(0)
  ok <- FALSE
  for (key in names(reg)) {
    bits <- as.integer(strsplit(key, "")[[1L]])
    if (length(bits) != length(sets)) next
    label <- paste(sets[bits == 1L], collapse = "&")
    poly <- reg[[key]]$pol %||% reg[[key]]$polygon %||% NULL
    if (is.null(poly)) next
    out[label] <- poly_area(list(list(x = poly$x, y = poly$y)))
    ok <- TRUE
  }
  if (!ok) NULL else out
}

# ---------------------------------------------------------------------------
# Benchmark inputs: disjoint combination vectors spanning set counts and
# difficulty. Several are derived from eulerr's bundled datasets.
# ---------------------------------------------------------------------------

combos_from <- function(x, ...) {
  # Reuse eulerr's input parsing to get a disjoint combination vector.
  euler(x, ...)$original.values
}

datasets <- list(
  "two-set" = c(A = 30, B = 20, "A&B" = 10),
  "three-set" = c(
    A = 40,
    B = 30,
    C = 20,
    "A&B" = 10,
    "A&C" = 8,
    "B&C" = 6,
    "A&B&C" = 3
  ),
  "fruits" = combos_from(eulerr::fruits[, 1:3]),
  "organisms" = combos_from(eulerr::organisms),
  "kinases" = c(
    agc = 9,
    camk = 17,
    cmgc = 16,
    tk = 16,
    tkl = 23,
    "agc&camk" = 1,
    "camk&tk" = 1,
    "tk&tkl" = 1,
    "camk&cmgc&tkl" = 1,
    "camk&tk&tkl" = 2,
    "agc&camk&tk&tkl" = 1,
    "camk&cmgc&tk&tkl" = 3,
    "agc&camk&cmgc&tk&tkl" = 1
  )
)

# ---------------------------------------------------------------------------
# Comparison plan. Each row pairs a competitor with the eulerr configuration
# that optimizes the *same* objective, so both are judged on the matching
# metric (the user's "per-loss" requirement). eulerr's default ellipse fit is
# included separately as a reference, labeled "eulerr (ellipse)".
# ---------------------------------------------------------------------------

comparisons <- list(
  list(
    comparison = "A: circles vs. venneuler",
    metric = "stress",
    eulerr_loss = "stress",
    eulerr_shape = "circle",
    competitor = "venneuler",
    fit_competitor = fit_venneuler,
    datasets = c("two-set", "three-set", "fruits", "organisms", "kinases")
  ),
  list(
    comparison = "B: region proportionality vs. nVennR",
    metric = "diagError",
    eulerr_loss = "diag_error",
    eulerr_shape = "circle",
    competitor = "nVennR",
    fit_competitor = fit_nvennr,
    datasets = c("three-set", "organisms", "kinases")
  ),
  list(
    comparison = "C: circles vs. BioVenn",
    metric = "stress",
    eulerr_loss = "stress",
    eulerr_shape = "circle",
    competitor = "BioVenn",
    fit_competitor = fit_biovenn,
    datasets = c("two-set", "three-set", "fruits")
  )
)

# ---------------------------------------------------------------------------
# Run.
# ---------------------------------------------------------------------------

acc_rows <- list()
time_rows <- list()
ran <- character(0)
skipped <- character(0)

add_row <- function(comparison, dataset, package, metric, value, time_ms) {
  acc_rows[[length(acc_rows) + 1L]] <<- data.frame(
    comparison = comparison,
    dataset = dataset,
    package = package,
    metric = metric,
    value = value,
    stringsAsFactors = FALSE
  )
  time_rows[[length(time_rows) + 1L]] <<- data.frame(
    comparison = comparison,
    dataset = dataset,
    package = package,
    time_ms = time_ms,
    stringsAsFactors = FALSE
  )
}

pick <- function(metrics, metric) {
  if (metric == "stress") metrics$stress else metrics$diagError
}

for (cmp in comparisons) {
  for (ds in cmp$datasets) {
    combos <- datasets[[ds]]
    n_sets <- length(set_names_of(combos))

    # eulerr, configured to optimize the competitor's objective.
    er <- tryCatch(
      fit_eulerr(combos, shape = cmp$eulerr_shape, loss = cmp$eulerr_loss),
      error = function(e) {
        message("eulerr failed on ", ds, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(er)) {
      add_row(
        cmp$comparison,
        ds,
        "eulerr (circle)",
        cmp$metric,
        pick(er$metrics, cmp$metric),
        er$time_ms
      )
    }

    # eulerr default (ellipse) reference, on the same metric.
    erl <- tryCatch(
      fit_eulerr(combos, shape = "ellipse", loss = cmp$eulerr_loss),
      error = function(e) NULL
    )
    if (!is.null(erl)) {
      add_row(
        cmp$comparison,
        ds,
        "eulerr (ellipse)",
        cmp$metric,
        pick(erl$metrics, cmp$metric),
        erl$time_ms
      )
    }

    # Competitor.
    co <- tryCatch(
      cmp$fit_competitor(combos),
      error = function(e) {
        message(cmp$competitor, " failed on ", ds, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(co)) {
      skipped <- union(skipped, paste0(cmp$competitor, " [", ds, "]"))
    } else {
      ran <- union(ran, cmp$competitor)
      add_row(
        cmp$comparison,
        ds,
        cmp$competitor,
        cmp$metric,
        pick(co$metrics, cmp$metric),
        co$time_ms
      )
    }
  }
}

accuracy <- do.call(rbind, acc_rows)
timing <- do.call(rbind, time_rows)

results <- list(
  accuracy = accuracy,
  timing = timing,
  meta = list(
    generated = format(Sys.time()),
    competitors_run = ran,
    competitors_skipped = skipped,
    sessionInfo = utils::capture.output(utils::sessionInfo())
  )
)

out_path <- "vignettes/benchmark_results.rds"
saveRDS(results, out_path)
message("Wrote ", out_path)
message("Competitors run: ", paste(ran, collapse = ", "))
if (length(skipped)) {
  message("Skipped: ", paste(skipped, collapse = ", "))
}

print(accuracy)
print(timing)
