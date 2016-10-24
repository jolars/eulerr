context("Test inputs to eulerr")

test_that("erroneous named numeric vectors returns errors", {
  expect_error(eulerr(c(A = 1)))
  expect_error(eulerr(c(1, 2, 3)))
  expect_error(eulerr(c()))
  expect_error(eulerr(c(A = FALSE, B = TRUE, C = FALSE)))
  expect_error(eulerr(c(A = 0, B = 2)))
  expect_error(eulerr(c(A = 10, A = 5)))
  expect_error(eulerr(c(A = 10, 4)))
  expect_error(eulerr(c(A = 10, B = 5, "A&B" = 6)))
  expect_warning(eulerr(c(A = 10, B = 5, "A&B" = 2), hello = "fwef"))
})

test_that("erroneous matrix returns errors", {
  expect_error(eulerr(cbind(A = TRUE, "&asdf" = FALSE)))
  expect_error(eulerr(cbind(A = "asfh", B = "qwer")))
})

test_that("erroneous input using by argument return errors", {
  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    y = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE),
    z = sample(c("asdf", "qwer", size = 100, replace = TRUE))
  )
  expect_error(eulerr(dat[, 1:2], by = dat[, 3:5]))
  expect_error(eulerr(dat[, 1:2], by = dat[1:50, 3]))
  expect_error(eulerr(dat[, 1:2], by = list(dat[, 2])))
  expect_error(eulerr(dat[, 1:2], by = 1:100))
  expect_error(eulerr(cbind(1:100, 1:100)))
})

