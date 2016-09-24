context("Test utility and other miscellaneous functions")

test_that("erroneous input to residual function return errors", {
  expect_error(eulerr:::residuals.eulerr(2))
})
