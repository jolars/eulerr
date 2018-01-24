context("Test plot functions")

test_that("erroneous input to plot.euler return errors", {
  f <- euler(c(A = 5, B = 2))
  expect_error(dont_plot(f, n = -1))
})

test_that("normal plotting works without errors", {
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_silent(dont_plot(f1,
                          fills = list(fill = c("black", "blue"), alpha = 0.3),
                          labels = list(font = 4)))

  expect_silent(dont_plot(f1))
  expect_silent(dont_plot(f1, legend = TRUE, quantities = TRUE))
  expect_silent(dont_plot(f1, legend = FALSE, quantities = TRUE))
  expect_silent(dont_plot(f1, legend = TRUE, labels = FALSE))
  expect_silent(dont_plot(f1, labels = c("asdf", "qwer")))

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat, by = list(Gender))
  f3 <- euler(dat, by = list(Gender, Nation))

  expect_silent(dont_plot(f2,
                          fills = list(fill = "transparent",
                                       lty = c(1, 2),
                                       lwd = c(1, 2))))
  expect_silent(dont_print(dont_plot(f3)))
})


