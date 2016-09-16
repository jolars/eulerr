library(assertthat)
context("Test inputs to eulerr")

test_that("erroneous input returns errors", {
  expect_error(eulerr(c(A = 1)))
  expect_error(eulerr(c(1, 2, 3)))
  expect_error(eulerr(c()))
  expect_error(eulerr(c(A = FALSE, B = TRUE, C = FALSE)))
})


