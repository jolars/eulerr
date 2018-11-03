context("Monte Carlo approximation")

test_that("Monte Carlo area approximation approximates the real solution", {
  s <- c(A = 1, B = 2, C = 3, "A&B" = 0.2, "A&C" = 0.1, "B&C" = 0.3,
         "A&B&C" = 0.01)
  fit <- euler(s)

  set.seed(1)

  exact <- eulerr:::intersect_ellipses(t(as.matrix(coef(fit))), FALSE, FALSE)
  approx <- eulerr:::intersect_ellipses(t(as.matrix(coef(fit))), FALSE, TRUE)

  expect_equal(exact, approx, tolerance = 1e-1)
})
