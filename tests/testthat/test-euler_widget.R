test_that("euler_widget() returns an htmlwidget", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("jsonlite")

  fit <- euler(c(A = 10, B = 5, "A&B" = 3))
  w <- euler_widget(fit)

  expect_s3_class(w, "htmlwidget")
  expect_true(inherits(w, "eulerr"))
  expect_type(w$x, "list")
  expect_named(
    w$x,
    c("regions", "edges", "labels", "container", "xlim", "ylim"),
    ignore.order = TRUE
  )
})

test_that("region count matches the non-empty regions", {
  skip_if_not_installed("htmlwidgets")

  fit <- euler(c(A = 10, B = 5, "A&B" = 3))
  w <- euler_widget(fit)
  expect_length(w$x$regions, 3L)

  labels <- vapply(w$x$regions, function(r) r$label, character(1))
  expect_setequal(labels, c("A", "B", "A&B"))

  # a disjoint fit has no intersection region to draw
  disjoint <- euler(c(A = 10, B = 10))
  wd <- euler_widget(disjoint)
  expect_length(wd$x$regions, 2L)
})

test_that("widget fills equal the static plot's resolved fills", {
  skip_if_not_installed("htmlwidgets")

  fit <- euler(c(A = 13, B = 8, C = 4, "A&B" = 3, "A&C" = 2, "B&C" = 1))
  w <- euler_widget(fit)

  # what plot.euler() would resolve, via the shared helper
  idx <- build_region_index(fit)
  rf <- resolve_region_fills(
    TRUE,
    list(),
    idx$id,
    idx$setnames,
    idx$n_e,
    idx$n_id,
    eulerr_options()
  )
  expected <- to_hex(rf$gp$fill)
  names(expected) <- idx$combo_labels

  for (r in w$x$regions) {
    expect_equal(r$fill, unname(expected[r$label]), info = r$label)
  }
})

test_that("venn objects dispatch and show quantities by default", {
  skip_if_not_installed("htmlwidgets")

  v <- venn(list(A = 1:5, B = 4:8))
  w <- euler_widget(v)

  expect_s3_class(w, "htmlwidget")
  # quantities on by default for venn -> visible labels carry text
  expect_gt(length(w$x$labels), 0L)
})

test_that("the container/complement box is emitted when requested", {
  skip_if_not_installed("htmlwidgets")

  fit <- euler(c(A = 10, B = 7, "A&B" = 3), complement = 20)
  w <- euler_widget(fit)

  expect_false(is.null(w$x$container))
  expect_length(w$x$container$outline_x, 5L)
  expect_equal(w$x$container$lty, 2L)
  expect_equal(w$x$container$label_text, "20")

  # suppressed when complement = FALSE
  w2 <- euler_widget(fit, complement = FALSE)
  expect_null(w2$x$container)
})

test_that("faceted diagrams raise a clear error", {
  skip_if_not_installed("htmlwidgets")

  dat <- data.frame(
    A = c(TRUE, FALSE, TRUE, FALSE),
    B = c(TRUE, TRUE, FALSE, FALSE),
    g = c("x", "y", "x", "y")
  )
  faceted <- euler(dat, by = list(g))

  expect_error(euler_widget(faceted), "does not support faceted")
})

test_that("fills = FALSE keeps regions as hover targets", {
  skip_if_not_installed("htmlwidgets")

  fit <- euler(c(A = 10, B = 5, "A&B" = 3))
  w <- euler_widget(fit, fills = FALSE)

  expect_length(w$x$regions, 3L)
  # no resolved fill color, but geometry is still present for hover
  expect_null(w$x$regions[[1]]$fill)
  expect_true(length(w$x$regions[[1]]$x) > 0L)
})

test_that("the payload serializes to JSON", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("jsonlite")

  fit <- euler(c(A = 10, B = 7, "A&B" = 3), complement = 20)
  w <- euler_widget(fit)
  expect_silent(js <- htmlwidgets:::toJSON2(w$x))
  expect_gt(nchar(js), 0L)
})

test_that("to_hex() converts R colors, including transparent", {
  expect_equal(to_hex("black"), "#000000")
  expect_equal(to_hex(1L), "#000000")
  expect_equal(to_hex("transparent"), "transparent")
  expect_equal(to_hex(c("white", NA)), c("#FFFFFF", "transparent"))
})
