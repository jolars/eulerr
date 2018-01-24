context("Test inputs to euler")

test_that("erroneous named numeric vectors returns errors", {
  expect_error(euler(c(1, 2, 3)))
  expect_error(euler(c()))
  expect_error(euler(c(A = FALSE, B = TRUE, C = FALSE)))
  expect_error(euler(c(A = 0, B = 2)))
  expect_error(euler(c(A = 10, A = 5)))
  expect_error(euler(c(A = 10, 4)))
})

test_that("erroneous matrix returns errors", {
  expect_error(euler(cbind(A = TRUE, "&asdf" = FALSE)))
})

test_that("erroneous input using by argument return errors", {
  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    y = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE),
    z = sample(c("asdf", "qwer", size = 100, replace = TRUE))
  )

  expect_error(euler(dat, by = list(x, y, z)))
  expect_error(euler(dat, by = dat[1:50, 3]))
  expect_error(euler(dat, by = list(dat[, 2])))
  expect_error(euler(dat, by = 1:100))
})

test_that("arguments to print.euler are specified correctly", {
  f <- euler(c(A = 10.923, B = 5.4, "A&B" = 0.43))
  expect_error(dont_print(f, round = "hello"))
  expect_error(dont_print(f, round = c(1, 2)))
})

test_that("normal use returns no errors", {
  f <- euler(c(A = 10.923, B = 5.4, "A&B" = 0.43))

  expect_error(dont_print(f, round = 2), NA)

  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE)
  )

  expect_error(euler(dat), NA)
  expect_error(euler(as.matrix(dat[, 1:2])), NA)
  expect_error(euler(dat, by = x), NA)
  expect_error(dont_print(euler(dat, by = x)), NA)
})

test_that("impossible configurations throw errors", {
  expect_error(euler(c(A = 10, B = 14, "A&B" = 15), input = "union"))
})
