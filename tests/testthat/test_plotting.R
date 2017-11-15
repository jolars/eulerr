context("Test plot functions")

test_that("erroneous input to plot.euler return errors", {
  f <- euler(c(A = 5, B = 2))
  expect_error(dont_plot(f, fill_alpha = "f"))
  expect_error(dont_plot(f, fill_alpha = c(2, 3)))
})

test_that("normal plotting works without errors", {
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_error(dont_plot(
    f1,
    fill = c("black", "blue"),
    labels = c("Yes", "No"),
    fill_alpha = 0.3,
    fontface = 4
  ), NA)

  expect_error(dont_plot(f1), NA)
  expect_error(dont_plot(f1, auto.key = TRUE, quantities = TRUE), NA)
  expect_error(dont_plot(f1, labels = c("asdf", "qwer")), NA)

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:2], by = dat[, 3:4])
  f3 <- euler(dat[, 1:2], by = dat[, 3, drop = FALSE])

  expect_error(dont_plot(f2,
                         fill = "transparent",
                         lty = c(1, 2),
                         lwd = c(1, 2)), NA)
  expect_error(dont_plot(f3), NA)
})


