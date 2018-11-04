context("Inputs")

test_that("normal use returns no errors", {
  f <- euler(c(A = 10.923, B = 5.4, "A&B" = 0.43))

  expect_error(dont_print(f, round = 2), NA)

  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE)
  )

  expect_is(euler(dat), "euler")
  expect_silent(euler(as.matrix(dat[, 1:2])))
  expect_is(euler(dat, by = x), "euler")
  expect_silent(dont_print(euler(dat, by = x)))
})

test_that("using weights works", {
  dat2 <- data.frame(A = c(TRUE, FALSE, TRUE, TRUE),
                     B = c(FALSE, TRUE, TRUE, FALSE))
  expect_is(euler(dat2, weights = c(3, 2, 1, 1)), "euler")
})

test_that("correct parallel options are asserted", {
  s <- c(a = 1, b = 2, "a&b" = 3)
  expect_silent(euler(s, n_threads = 1))
})

test_that("zero-sized input is allowed", {
  s <- c(A = 0)
  expect_silent(f <- euler(s))
  expect_equivalent(fitted(f), 0)

  s <- c(A = 0, B = 0)
  expect_silent(f <- euler(s))
  expect_equivalent(fitted(f), rep(0, length(fitted(f))))
})

test_that("factors in euler.data.frame() are handled appropriately", {
  d <- data.frame(A = sample(c(TRUE, FALSE), 90, TRUE),
                  gender = gl(3, 30, 90, c("man", "woman", "transgender")),
                  status = sample(gl(2, 45, 90, c("cohabitation", "single"))))
  expect_silent(f <- euler(d, by = list(status)))
})

test_that("list method works appropriately", {
  l <- list(A = c("a", "b", "c"),
            B = c("b", "e"),
            C = c("c"))
  expect_silent(euler(l))
})

test_that("table method works appropriately", {
  expect_silent(euler(as.table(apply(Titanic, 2:4, sum))))
})
