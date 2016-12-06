context("Test plot functions")

test_that("erroneous input to plot.eulerr return errors", {
  f <- eulerr(c(A = 5, B = 2))
  expect_error(plot(f, mar = c(2, 2, 2)))
  expect_error(plot(f, mar = "Hello"))
  expect_error(plot(f, fill_opacity = "f"))
  expect_error(plot(f, fill_opacity = c(2, 3)))
  expect_error(plot(f, polygon_args = 1))
  expect_error(plot(f, text_args = 1))
  class(f) <- "list"
  expect_error(plot(f))
})

test_that("erroneous input to plot.eulerr_grid return errors", {
  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    y = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE),
    z = sample(c("asdf", "qwer", size = 100, replace = TRUE))
  )
  f2 <- eulerr(dat[, 1:2], by = dat[, 3:4])
  expect_error(plot(f2, mfrow = 1))
  expect_error(plot(f2, mfrow = c(1, 2, 3)))
  expect_error(plot(f2, mfrow = "asdf"))
  expect_error(plot(f2, main = 1))
  expect_error(plot(f2, main = c("aa", "a")))

  class(f2) <- "eulerr"
})
