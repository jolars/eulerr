context("Test plot functions")

test_that("erroneous input to plotting functions return errors", {
  expect_error(eulerr:::plot.eulerr(2))
  expect_error(eulerr:::residuals.eulerr(2))
})
