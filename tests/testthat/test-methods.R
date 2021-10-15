test_that("fitted and coef methods work as they should", {
  fit <- euler(c(A = 1, B = 2))
  expect_is(fitted(fit), "numeric")
  expect_is(coef(fit), "data.frame")
})
