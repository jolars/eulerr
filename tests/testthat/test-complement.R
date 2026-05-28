test_that("complement attaches a fitted container", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)

  expect_false(is.null(fit$container))
  expect_equal(fit$container$complement, 145)

  container_area <- fit$container$width * fit$container$height
  fitted_total <- sum(fit$fitted.values)
  # Container area minus union of fitted shapes ≈ requested complement.
  expect_equal(
    container_area - fitted_total,
    145,
    tolerance = 1e-2
  )
  expect_equal(fit$container$complement_fitted, 145, tolerance = 1e-2)
})

test_that("no complement leaves container NULL", {
  fit <- euler(c(A = 5, B = 5, "A&B" = 1))
  expect_null(fit$container)
})

test_that("complement validates its input", {
  expect_error(
    euler(c(A = 1, B = 1, "A&B" = 0.5), complement = -1),
    "non-negative"
  )
  expect_error(
    euler(c(A = 1, B = 1, "A&B" = 0.5), complement = c(1, 2)),
    "single non-negative"
  )
  expect_error(
    euler(c(A = 1, B = 1, "A&B" = 0.5), complement = NA_real_),
    "non-negative"
  )
})

test_that("complement is rejected for venn()", {
  expect_error(
    venn(c(A = 1, B = 1, "A&B" = 0.5), complement = 5),
    "not supported for `venn"
  )
})

test_that("single-set complement synthesises a container", {
  fit <- euler(c(A = 30), complement = 70)
  expect_false(is.null(fit$container))
  # universe area = 30 + 70 = 100, square side = 10
  expect_equal(fit$container$width, 10, tolerance = 1e-6)
  expect_equal(fit$container$height, 10, tolerance = 1e-6)
})

test_that("plotting an euler with complement renders without error", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({
    dev.off()
    unlink(tmp)
  })
  expect_silent(plot(fit, quantities = TRUE))
  expect_silent(plot(fit, complement = FALSE))
  expect_silent(plot(fit, complement = "grey90"))
  expect_silent(plot(
    fit,
    complement = list(
      fill = "lightyellow",
      col = "navy",
      lty = 1,
      lwd = 2,
      label = "outside",
      fontsize = 14,
      font = 2
    )
  ))
})

diagram_children <- function(g) {
  # Walk the diagram subtree and return a flat list keyed by grob name.
  # The complement label moved inside the EulerTags subtree so the
  # complement count can be re-placed at draw time alongside region
  # tags; the legacy lookup path returned only the top-level children
  # of `diagram.grob.<n>`.
  diagram <- g$children$canvas.grob$children[[1]]
  out <- list()
  walk <- function(node) {
    nm <- node$name
    if (!is.null(nm) && nzchar(nm)) {
      out[[nm]] <<- node
    }
    kids <- node$children
    if (!is.null(kids)) {
      for (i in seq_along(kids)) walk(kids[[i]])
    }
  }
  walk(diagram)
  out
}

test_that("complement = FALSE drops the container grobs", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({
    dev.off()
    unlink(tmp)
  })
  with_box <- plot(fit, quantities = TRUE)
  no_box <- plot(fit, quantities = TRUE, complement = FALSE)

  expect_true("container.edge.grob" %in% names(diagram_children(with_box)))
  expect_false("container.edge.grob" %in% names(diagram_children(no_box)))
  expect_false(
    "complement.quantity.grob" %in% names(diagram_children(no_box))
  )
})

test_that("complement$label overrides the count text", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({
    dev.off()
    unlink(tmp)
  })
  g <- plot(fit, complement = list(label = "n = 145"))
  label_grob <- diagram_children(g)[["complement.quantity.grob"]]
  expect_false(is.null(label_grob))
  expect_equal(label_grob$label, "n = 145")
})

test_that("complement gpar overrides flow through to the outline grob", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({
    dev.off()
    unlink(tmp)
  })
  g <- plot(
    fit,
    complement = list(col = "navy", lty = 1, lwd = 3, fill = "lightyellow")
  )
  edge <- diagram_children(g)[["container.edge.grob"]]
  fill <- diagram_children(g)[["complement.fill.grob"]]
  expect_equal(edge$gp$col, "navy")
  expect_equal(edge$gp$lty, 1)
  expect_equal(edge$gp$lwd, 3)
  expect_equal(fill$gp$fill, "lightyellow")
})

test_that("default complement uses dashed outline (lty = 2)", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  tmp <- tempfile(fileext = ".png")
  png(tmp)
  on.exit({
    dev.off()
    unlink(tmp)
  })
  g <- plot(fit)
  edge <- diagram_children(g)[["container.edge.grob"]]
  expect_equal(edge$gp$lty, 2L)
})

test_that("euler_plot_data returns a complement polygon and label", {
  set.seed(1)
  fit <- euler(c(A = 25, B = 25, "A&B" = 5), complement = 145)
  shapes <- fit$shapes
  cd <- euler_plot_data(
    set_names = rownames(shapes),
    shape = shapes$type[1L],
    h = shapes$h,
    k = shapes$k,
    a = shapes$a,
    b = shapes$b,
    phi = shapes$phi,
    width = shapes$width,
    height = shapes$height,
    side = shapes$side,
    container_h = fit$container$h,
    container_k = fit$container$k,
    container_width = fit$container$width,
    container_height = fit$container$height,
    n_vertices = 200L,
    label_precision = 0.1
  )
  expect_true(cd$has_complement)
  expect_length(cd$container_outline_x, 5L)
  expect_length(cd$container_outline_y, 5L)
  expect_gt(length(cd$complement_polygon$x), 0L)
  # Label anchor must lie outside both fitted disks.
  d_to_A <- sqrt(
    (cd$complement_label_x - shapes$h[1])^2 +
      (cd$complement_label_y - shapes$k[1])^2
  )
  d_to_B <- sqrt(
    (cd$complement_label_x - shapes$h[2])^2 +
      (cd$complement_label_y - shapes$k[2])^2
  )
  expect_gt(d_to_A, shapes$a[1])
  expect_gt(d_to_B, shapes$a[2])
})
