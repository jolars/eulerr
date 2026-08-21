find_grob_class <- function(x, class) {
  if (inherits(x, class)) {
    return(x)
  }
  if (is.null(x$children)) {
    return(NULL)
  }
  for (child in x$children) {
    found <- find_grob_class(child, class)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

test_that("unit glyphs are packed and rendered", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(fit, glyphs = TRUE)
  glyphs <- find_grob_class(plot, "EulerGlyphs")

  expect_s3_class(glyphs, "EulerGlyphs")
  expect_identical(glyphs$mode, "dots")
  expect_equal(glyphs$values, c(A = 3, B = 2, "A&B" = 1))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("glyph counts must be nonnegative integers", {
  fit <- euler(c(A = 1.5, B = 2, "A&B" = 1))
  expect_error(plot(fit, glyphs = TRUE), "nonnegative integers")

  fit <- euler(c(A = 2, B = 2, "A&B" = 1))
  expect_error(
    plot(fit, glyphs = list(counts = c(A = -1))),
    "nonnegative integers"
  )
  expect_error(
    plot(fit, glyphs = list(counts = c(C = 1))),
    "Unknown set"
  )
  expect_error(
    plot(fit, glyphs = list(counts = c("A&B" = 1, "B&A" = 1))),
    "duplicate regions"
  )
})

test_that("glyph safety limits skip oversized layers", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  expect_warning(
    plot(fit, glyphs = list(max_items = 2)),
    "exceed `glyphs\\$max_items`"
  )
})

test_that("member labels are packed at draw time", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(
    fit,
    glyphs = list(
      mode = "members",
      labels = list(A = c("Ada", "Grace"), B = "Alan", "A&B" = "Kay")
    )
  )
  glyphs <- find_grob_class(plot, "EulerGlyphs")

  expect_identical(glyphs$mode, "members")
  expect_equal(unname(lengths(glyphs$values)), c(2L, 1L, 1L))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("outside set labels use their own placement layer", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(
    fit,
    labels = list(position = "outside"),
    quantities = TRUE
  )
  set_labels <- find_grob_class(plot, "EulerSetLabels")

  expect_s3_class(set_labels, "EulerSetLabels")
  expect_true(all(is.na(plot$data$centers$labels)))
  expect_equal(plot$data$centers$quantities, c("3", "2", "1"))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("glyph controls are validated", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  expect_error(plot(fit, glyphs = list(gap = -1)), "glyphs\\$gap")
  expect_error(
    plot(fit, glyphs = list(arrangement = "spiral")),
    "one of"
  )
  expect_error(
    plot(fit, glyphs = list(mode = "members", labels = c(A = "Ada"))),
    "fully named list"
  )
  expect_error(
    plot(fit, labels = list(position = "outside", angular_steps = 4)),
    "at least 8"
  )
})
