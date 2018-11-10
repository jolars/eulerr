context("Assertions")

test_that("erroneous input to euler() throw errors", {
  set.seed(1)

  expect_error(euler(c(1, 2, 3)))
  expect_error(euler(c()))
  expect_error(euler(c(A = FALSE, B = TRUE, C = FALSE)))
  expect_error(euler(c(A = 10, A = 5)))
  expect_error(euler(c(A = 10, 4)))
  expect_error(euler(cbind(A = TRUE, "&asdf" = FALSE)))

  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    y = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE),
    z = sample(c("asdf", "qwer"), size = 100, replace = TRUE)
  )

  expect_error(euler(dat, by = list(x, y, z)))
  expect_error(euler(dat, by = dat[1:50, 3]))
  expect_error(euler(dat, by = list(dat[, 2])))

  expect_error(euler(cbind(dat, rnorm(nrow(dat)))))

  # impossible configuration
  expect_error(euler(c(A = 10, B = 14, "A&B" = 15), input = "union"))

  # list method
  expect_error(euler(list(c("a", "b"), c("a", "c"))))
  expect_error(euler(list(b = c(1, 2), b = c(1, 3))))
})

test_that("erroneous input to venn() throws errors", {
  expect_error(venn(2))
  expect_error(venn(0))
  expect_error(venn(2, names = "a"))
  expect_error(venn(6, names = letters[1:n]))
})

test_that("erroneous input to print.euler() throw errors", {
  f <- euler(c(A = 10.923, B = 5.4, "A&B" = 0.43))
  expect_error(dont_print(f, round = "hello"))
  expect_error(dont_print(f, round = c(1, 2)))
})

test_that("erroneous input to plot.euler() return errors", {
  f <- euler(c(A = 5, B = 2))
  expect_error(dont_plot(f, n = -1))
})

test_that("erroneous input to error_plot() throws", {
  f <- euler(organisms)

  expect_error(error_plot(f, fills = list(col = "grey")))
  expect_error(error_plot(f, legend = TRUE))
  expect_error(error_plot(f, strips = TRUE))
})
