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

test_that("normal plotting works without errors", {
  expect_error(
    plot(
      eulerr(c("A" = 10, "B" = 5, "A&B" = 2)),
      fill_opacity = 0.3,
      polygon_args = list(col = c(1 ,2)),
      text_args = list(font = 4),
      mar = c(1, 2, 1, 2)
    ),
    NA
  )

  dat <- data.frame(
    A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  expect_error(
    plot(
      eulerr(dat[, 1:2], by = dat[, 3:4]),
      polygon_args = list(col = "transparent"),
      mfrow = c(1, 4),
      main = c("A", "B", "C", "D")
    ),
    NA
  )
})
