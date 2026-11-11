# Run with Rscript data-raw/test-benchmarks.R in the development environment.
library(testthat)

benchmark_helpers <- function() {
  env <- new.env()
  # Load helpers without running benchmarks or replacing stored results.
  for (expr in parse("data-raw/benchmarks.R")) {
    if (
      is.call(expr) &&
        identical(expr[[1L]], as.name("<-")) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      eval(expr, envir = env)
    }
  }
  # These tests exercise geometry extraction without taking timing measurements.
  env$time_ms <- function(expr, reps) 0
  env
}

test_that("the comparison corpus converts exactly to membership lists", {
  helpers <- benchmark_helpers()
  source("data-raw/benchmark-cases.R", local = helpers)
  cases <- helpers$case_info
  inputs <- helpers$datasets
  expect_equal(nrow(cases), 8L)
  expect_identical(cases$dataset, names(inputs))
  expect_equal(cases$n_sets, c(3L, 3L, 3L, 3L, 4L, 4L, 5L, 6L))
  for (name in names(inputs)) {
    combos <- inputs[[name]]
    sets <- helpers$set_names_of(combos)
    expect_length(sets, cases$n_sets[match(name, cases$dataset)])
    expect_true(all(is.finite(combos) & combos > 0 & combos == round(combos)))
    lists <- helpers$combos_to_lists(combos)
    expect_length(unique(unlist(lists)), sum(combos))
    for (set in sets) {
      belongs <- vapply(
        strsplit(names(combos), "&", fixed = TRUE),
        function(members) set %in% members,
        logical(1)
      )
      expect_length(lists[[set]], sum(combos[belongs]))
    }
  }
})

test_that("nVennR boundaries yield the areas of the drawn polygons", {
  helpers <- benchmark_helpers()
  svg <- paste0(
    '<svg><symbol id="bl1"><path d="M 1 0 L 4 0 L 4 2 L 1 2 Z" /></symbol>',
    '<symbol id="bl0"><path d="M 0 0 L 2 0 L 2 2 L 0 2 Z" /></symbol></svg>'
  )
  expect_equal(
    helpers$nvennr_region_areas(svg, c("A", "B")),
    c(A = 2, B = 4, "A&B" = 2)
  )
  expect_error(
    helpers$nvennr_region_areas(svg, c("A", "B", "C")),
    "missing set boundaries"
  )
  expect_error(
    helpers$nvennr_region_areas(
      sub("L 2 0", "C 2 0", svg, fixed = TRUE),
      c("A", "B")
    ),
    "Unsupported curve commands"
  )
})

test_that("nVennR rendered output can be scored", {
  helpers <- benchmark_helpers()
  sets <- list(A = 1:6, B = 4:8, C = c(2, 4, 7, 9))
  obj <- nVennR::plotVenn(sets, showPlot = FALSE, nCycles = 20)
  svg <- getFromNamespace("refineVenn", "nVennR")(obj)
  areas <- helpers$nvennr_region_areas(svg, names(sets))
  expect_setequal(names(areas), c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C"))
  expect_true(all(is.finite(areas)))
  expect_gt(sum(areas), 0)
})

test_that("BioVenn circles are scored quietly without modifying the package", {
  helpers <- benchmark_helpers()
  original <- body(BioVenn::draw.venn)
  sinks <- sink.number()
  device <- grDevices::dev.cur()
  result <- NULL
  expect_output(
    result <- helpers$fit_biovenn(c(A = 30, B = 20, "A&B" = 10)),
    NA
  )
  expect_lt(result$metrics$stress, 1e-4)
  expect_true(all(is.finite(unlist(result$metrics))))
  expect_identical(body(BioVenn::draw.venn), original)
  expect_equal(sink.number(), sinks)
  expect_equal(grDevices::dev.cur(), device)

  expect_output(
    result <- helpers$fit_biovenn(c(
      A = 40,
      B = 30,
      C = 20,
      "A&B" = 10,
      "A&C" = 8,
      "B&C" = 6,
      "A&B&C" = 3
    )),
    NA
  )
  expect_true(all(is.finite(unlist(result$metrics))))
})

test_that("BioVenn output and graphics devices are restored after errors", {
  helpers <- benchmark_helpers()
  helpers$biovenn_geometry <- function(...) stop("geometry failure")
  sinks <- sink.number()
  device <- grDevices::dev.cur()
  expect_error(
    helpers$fit_biovenn(c(A = 30, B = 20, "A&B" = 10)),
    "geometry failure"
  )
  expect_equal(sink.number(), sinks)
  expect_equal(grDevices::dev.cur(), device)
})
