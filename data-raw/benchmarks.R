# Benchmark eulerr against competing CRAN/Bioconductor area-proportional
# Euler/Venn fitters. This script is *not* part of the package build (the
# `data-raw/` directory is listed in `.Rbuildignore`). It is run manually to
# produce `vignettes/benchmark_results.rds`, which the `comparison.Rmd` vignette
# reads and displays. Keeping the computation here prevents vignette rendering
# from affecting the benchmark timings.
#
# From the package root:
#
#   devenv shell
#   Rscript data-raw/benchmarks.R
#
# All competitor packages must load before the benchmarks start, and results
# are saved only if every competitor ran successfully on at least one dataset.

library(eulerr)
library(bench)
library(venneuler)
library(nVennR)
library(BioVenn)

set.seed(20240611) # only affects synthetic ID ordering for BioVenn/nVennR

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
  res <- bench::mark(
    eval(expr, env),
    iterations = reps,
    check = FALSE,
    filter_gc = FALSE
  )
  as.numeric(res$median) * 1000
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
    if (n_el <= 0L) {
      next
    }
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
# Per-package fit wrappers return metrics from realized geometry and fit times.
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

  list(metrics = m, time_ms = t)
}

fit_venneuler <- function(combos, reps = 5L) {
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
  sets <- set_names_of(combos)
  if (length(sets) < 2L || length(sets) > 3L) {
    stop("BioVenn supports only two or three sets.")
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
  # BioVenn prints membership counts on every fit, including timed repetitions.
  output <- file(nullfile(), open = "w")
  sink(output)
  on.exit(
    {
      sink()
      close(output)
    },
    add = TRUE
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  geom <- biovenn_geometry(args, sets)
  t <- time_ms(do.call(BioVenn::draw.venn, args), reps)
  m <- score_circles(combos, geom$x, geom$y, geom$r, sets)
  list(metrics = m, time_ms = t)
}

# BioVenn returns members, not geometry. Change only the return value of a
# private copy; timed fits still call the unmodified package function.
biovenn_geometry <- function(args, sets) {
  draw <- BioVenn::draw.venn
  last <- length(body(draw))
  if (!identical(body(draw)[[last]][[1L]], as.name("return"))) {
    stop("Unsupported BioVenn draw.venn implementation.")
  }
  body(draw)[[last]] <- quote(list(
    x = c(x_h, y_h, z_h),
    y = c(x_v, y_v, z_v),
    r = c(x_r, y_r, z_r)
  ))
  geometry <- do.call(draw, args)
  geometry <- lapply(geometry, function(x) x[seq_along(sets)])
  if (any(!is.finite(unlist(geometry))) || any(geometry$r <= 0)) {
    stop("BioVenn produced invalid circle geometry.")
  }
  geometry
}

fit_nvennr <- function(combos, reps = 3L) {
  lists <- combos_to_lists(combos)
  render <- function() {
    obj <- nVennR::plotVenn(lists, showPlot = FALSE)
    # Refinement changes the boundaries, so score and time the rendered diagram.
    getFromNamespace("refineVenn", "nVennR")(obj)
  }
  svg <- render()
  t <- time_ms(render(), reps)
  areas <- nvennr_region_areas(svg, names(lists))
  m <- fit_metrics(expand_combos(combos), areas)
  list(metrics = m, time_ms = t)
}

# nVennR's SVG defines each set boundary as a polygon in a numbered symbol.
nvennr_region_areas <- function(svg, sets) {
  pattern <- '(?s)<symbol id="bl[0-9]+">.*?</symbol>'
  symbols <- regmatches(svg, gregexpr(pattern, svg, perl = TRUE))[[1L]]
  ids <- sub('(?s)^<symbol id="([^"]+)".*$', "\\1", symbols, perl = TRUE)
  paths <- sub('(?s).*<path[^>]* d="([^"]+)".*$', "\\1", symbols, perl = TRUE)
  paths <- setNames(paths, ids)[paste0("bl", seq_along(sets) - 1L)]
  if (anyNA(paths)) {
    stop("nVennR SVG is missing set boundaries.")
  }
  polygons <- lapply(paths, function(path) {
    if (grepl("[^MLZ0-9eE+.,[:space:]-]", path)) {
      stop("Unsupported curve commands in nVennR SVG.")
    }
    coords <- scan(text = gsub("[MLZ,]", " ", path), quiet = TRUE)
    if (
      length(coords) < 6L ||
        length(coords) %% 2L != 0L ||
        any(!is.finite(coords))
    ) {
      stop("Invalid polygon coordinates in nVennR SVG.")
    }
    list(
      x = coords[seq(1L, length(coords), 2L)],
      y = coords[seq(2L, length(coords), 2L)]
    )
  })
  region_areas(setNames(polygons, sets))
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
  if (!is.finite(value) || !is.finite(time_ms) || time_ms < 0) {
    stop("Invalid benchmark result for ", package, " on ", dataset, ".")
  }
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
    message(cmp$comparison, " [", ds, "]")

    # eulerr, configured to optimize the competitor's objective.
    er <- fit_eulerr(combos, shape = cmp$eulerr_shape, loss = cmp$eulerr_loss)
    add_row(
      cmp$comparison,
      ds,
      "eulerr (circle)",
      cmp$metric,
      pick(er$metrics, cmp$metric),
      er$time_ms
    )

    # eulerr default (ellipse) reference, on the same metric.
    erl <- fit_eulerr(combos, shape = "ellipse", loss = cmp$eulerr_loss)
    add_row(
      cmp$comparison,
      ds,
      "eulerr (ellipse)",
      cmp$metric,
      pick(erl$metrics, cmp$metric),
      erl$time_ms
    )

    # Competitor.
    co <- cmp$fit_competitor(combos)
    add_row(
      cmp$comparison,
      ds,
      cmp$competitor,
      cmp$metric,
      pick(co$metrics, cmp$metric),
      co$time_ms
    )
    ran <- union(ran, cmp$competitor)
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
missing_competitors <- setdiff(
  vapply(comparisons, function(cmp) cmp$competitor, character(1)),
  ran
)
if (length(missing_competitors) > 0L) {
  stop(
    "No successful benchmarks for: ",
    paste(missing_competitors, collapse = ", "),
    ". Benchmark results were not saved."
  )
}
saveRDS(results, out_path)
message("Wrote ", out_path)
message("Competitors run: ", paste(ran, collapse = ", "))
if (length(skipped)) {
  message("Skipped: ", paste(skipped, collapse = ", "))
}

print(accuracy)
print(timing)
