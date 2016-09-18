context("Test plot functions")

test_that("erroneous input to plotting functions return errors", {
  expect_error(plot.eulerr(2))
})
