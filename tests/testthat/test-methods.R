test_that("fitted and coef methods work as they should", {
  fit <- euler(c(A = 1, B = 2))
  expect_is(fitted(fit), "numeric")
  expect_is(coef(fit), "data.frame")
})

test_that("coef returns $shapes (canonical) for all shape kinds", {
  combo <- c(A = 5, B = 3, "A&B" = 1)
  for (shape in c("circle", "ellipse", "rectangle", "square")) {
    fit <- euler(combo, shape = shape)
    out <- coef(fit)
    expect_is(out, "data.frame")
    expect_true("type" %in% names(out))
    expect_identical(out, fit$shapes)
  }
})

test_that("$ellipses is populated for circle/ellipse and absent otherwise", {
  combo <- c(A = 5, B = 3, "A&B" = 1)
  fc <- euler(combo, shape = "circle")
  fe <- euler(combo, shape = "ellipse")
  fr <- euler(combo, shape = "rectangle")
  fs <- euler(combo, shape = "square")
  expect_false(is.null(fc$ellipses))
  expect_false(is.null(fe$ellipses))
  expect_null(fr$ellipses)
  expect_null(fs$ellipses)
})
