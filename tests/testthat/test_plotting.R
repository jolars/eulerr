context("Test plot functions")

test_that("erroneous input to plot.euler return errors", {
  f <- euler(c(A = 5, B = 2))
  expect_error(plot(f, mar = c(2, 2, 2)))
  expect_error(plot(f, mar = "Hello"))
  expect_error(plot(f, fill_opacity = "f"))
  expect_error(plot(f, fill_opacity = c(2, 3)))
  expect_error(plot(f, polygon_args = 1))
  expect_error(plot(f, text_args = 1))
})

test_that("erroneous input to plot.euler_grid return errors", {
  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    y = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE),
    z = sample(c("asdf", "qwer", size = 100, replace = TRUE))
  )
  f2 <- euler(dat[, 1:2], by = dat[, 3:4])
  expect_error(plot(f2, mfrow = c(1, 2, 3)))
  expect_error(plot(f2, mfrow = "asdf"))
  expect_error(plot(f2, main = 1))
  expect_error(plot(f2, main = c("aa", "a")))
})

test_that("normal plotting works without errors", {
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))
  ff <- tempfile()
  png(filename = ff)

  expect_error(
    plot(
      f1,
      fill_opacity = 0.3,
      text_args = list(font = 4),
      mar = c(1, 2, 1, 2)
    ),
    NA
  )

  dev.off()
  unlink(ff)

  dat <- data.frame(
    A      = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B      = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:2], by = dat[, 3:4])

  ff <- tempfile()
  png(filename = ff)

  expect_error(
    plot(
      f2,
      polygon_args = list(col = "transparent"),
      mfrow = c(1, 4),
      main = c("A", "B", "C", "D")
    ),
    NA
  )

  dev.off()
  unlink(ff)
})
