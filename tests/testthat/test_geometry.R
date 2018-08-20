context("Geometry")

test_that("check that disc separation optimization works", {
  set.seed(1)

  r1 <- 5
  r2 <- 5
  tot <- r1^2*pi + r2^2*pi
  tol <- 1e-6

  expect_equal(eulerr:::separate_two_discs(r1, r2, 0), 10, tolerance = tol)
  expect_equal(eulerr:::separate_two_discs(r1, r2, tot), 0, tolerance = tol)
  expect_equal(eulerr:::separate_two_discs(r1, 0, 0), 0)
})
