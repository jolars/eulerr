test_that("rectangle fit populates width/height and leaves a/b/phi as NaN", {
  combo <- c(A = 5, B = 4, C = 6, "A&B" = 2, "A&C" = 1, "B&C" = 1, "A&B&C" = 0.5)
  fit <- euler(combo, shape = "rectangle")
  expect_is(fit, "euler")
  s <- fit$shapes
  expect_identical(unique(s$type), "rectangle")
  expect_true(all(is.finite(s$h)))
  expect_true(all(is.finite(s$k)))
  expect_true(all(is.finite(s$width)))
  expect_true(all(is.finite(s$height)))
  expect_true(all(is.nan(s$a)))
  expect_true(all(is.nan(s$b)))
  expect_true(all(is.nan(s$phi)))
  expect_true(all(is.nan(s$side)))
  expect_true(is.finite(fit$diagError))
  expect_null(fit$ellipses)
})

test_that("square fit populates side with width == height == side", {
  combo <- c(A = 5, B = 4, "A&B" = 1)
  fit <- euler(combo, shape = "square")
  s <- fit$shapes
  expect_identical(unique(s$type), "square")
  expect_true(all(is.finite(s$side)))
  expect_equal(s$width, s$side)
  expect_equal(s$height, s$side)
  expect_true(all(is.nan(s$a)))
})

test_that("rectangle/square diagrams plot without error", {
  combo <- c(A = 5, B = 4, "A&B" = 1)
  for (shape in c("rectangle", "square")) {
    fit <- euler(combo, shape = shape)
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    g <- plot(fit)
    expect_s3_class(g, "eulergram")
  }
})

test_that("single-set edge cases return well-defined geometry per shape", {
  for (shape in c("circle", "ellipse", "rectangle", "square")) {
    fit <- euler(c(A = 5), shape = shape)
    s <- fit$shapes
    expect_equal(NROW(s), 1L)
    expect_true(is.finite(s$h))
    expect_true(is.finite(s$k))
    if (shape %in% c("circle", "ellipse")) {
      expect_true(is.finite(s$a))
      expect_true(is.finite(s$b))
    } else {
      expect_true(is.finite(s$width))
      expect_true(is.finite(s$height))
    }
  }
})
