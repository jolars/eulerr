test_that("normal use of euler() returns no errors", {
  f <- euler(c(A = 10.923, B = 5.4, "A&B" = 0.43))

  expect_error(dont_print(f, round = 2), NA)

  # test both against stringsAsFactors = TRUE and FALSE
  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    stringsAsFactors = TRUE
  )

  expect_is(euler(dat), "euler")

  dat <- data.frame(
    A = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    B = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    x = sample(c("Men", "Women"), size = 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expect_is(euler(dat), "euler")

  expect_silent(euler(as.matrix(dat[, 1:2])))
  expect_is(euler(dat, by = x), "euler")
  expect_silent(dont_print(euler(dat, by = x)))
  expect_silent(dont_print(venn(dat, by = x)))

  d <- data.frame(
    Apple = c(1, 0, 0, 1),
    Banana = c(0, 1, 0, 1),
    Orange = c(1, 0, 1, 0)
  )
  expect_silent(euler(d))
})

test_that("normal use of venn() returns no errors", {
  expect_is(venn(organisms), "eulerr_venn")
  expect_silent(venn(organisms))
  expect_silent(venn(c(A = 1, B = 2)))
  expect_silent(venn(pain))
  expect_silent(venn(fruits, by = list(sex, age)))
  expect_silent(venn(organisms, weights = c(10, 20, 5, 4, 8, 9, 2)))
  expect_silent(venn(plants[c("erigenia", "solanum", "cynodon")]))
})

test_that("all venn diagram sizes work", {
  for (n in 2:5) {
    expect_silent(venn(n, names = letters[1:n]))
  }
})

test_that("normal use of venn() returns no errors", {
  expect_silent(venn(organisms))
  expect_silent(venn(c(A = 1, B = 2)))
  expect_silent(venn(pain))
  expect_silent(venn(fruits, by = list(sex, age)))
  expect_silent(venn(organisms, weights = c(10, 20, 5, 4, 8, 9, 2)))
  expect_silent(venn(plants[c("erigenia", "solanum", "cynodon")]))
})

test_that("all venn diagram sizes work", {
  for (n in 2:5) {
    expect_silent(venn(n, names = letters[1:n]))
  }
})

test_that("using weights works", {
  dat2 <- data.frame(
    A = c(TRUE, FALSE, TRUE, TRUE),
    B = c(FALSE, TRUE, TRUE, FALSE)
  )
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
  d <- data.frame(
    A = sample(c(TRUE, FALSE), 90, TRUE),
    gender = gl(3, 30, 90, c("man", "woman", "transgender")),
    status = sample(gl(2, 45, 90, c("cohabitation", "single")))
  )
  expect_silent(f <- euler(d, by = list(status)))
})

test_that("list method works appropriately", {
  l <- list(A = c("a", "b", "c"), B = c("b", "e"), C = c("c"))
  expect_silent(euler(l))
})

test_that("table method works appropriately", {
  expect_silent(euler(as.table(apply(Titanic, 2:4, sum))))
})

test_that("sparse inputs with many sets do not blow up", {
  # 12 sets with only a few input overlaps. Pre-refactor this allocated a
  # 4095-row id matrix and 4095-length result vectors per call.
  sets <- paste0("S", 1:12)
  input <- c(
    stats::setNames(rep(10, 12), sets),
    c("S1&S2" = 3, "S2&S3" = 2, "S1&S2&S3" = 1)
  )
  f <- euler(input)
  expect_lt(length(f$fitted.values), 50L)
  expect_lt(length(f$original.values), 50L)
  expect_true(all(c("S1", "S1&S2", "S1&S2&S3") %in% names(f$fitted.values)))
  expect_silent(plot(f))
})

test_that("legacy loss / loss_aggregator arguments warn and still work", {
  s <- c(A = 2, B = 2, "A&B" = 1)

  # New-style loss values are silent
  expect_silent(euler(s, loss = "sum_squared"))
  expect_silent(euler(s, loss = "diag_error"))

  # Legacy loss values warn and translate
  expect_warning(euler(s, loss = "square"), "deprecated")
  expect_warning(euler(s, loss = "region"), "deprecated")

  # loss_aggregator on its own warns and is otherwise ignored
  expect_warning(
    euler(s, loss = "sum_squared", loss_aggregator = "sum"),
    "loss_aggregator.*deprecated"
  )

  # Legacy combination produces the same fit as its new equivalent
  set.seed(1)
  new_fit <- euler(s, loss = "diag_error")
  set.seed(1)
  legacy_fit <- suppressWarnings(
    euler(s, loss = "region", loss_aggregator = "max")
  )
  expect_equal(new_fit$ellipses, legacy_fit$ellipses)
})

test_that("fitted(dense = TRUE) expands to all 2^n - 1 combinations", {
  f <- euler(c(A = 1, B = 1, "A&B" = 0.5))
  sparse <- fitted(f)
  dense <- fitted(f, dense = TRUE)
  expect_setequal(names(dense), c("A", "B", "A&B"))
  expect_identical(dense[names(sparse)], sparse)

  # With absent combinations, dense fills 0.
  f2 <- euler(c(A = 1, B = 1, C = 1, "A&B" = 0.5))
  dense2 <- fitted(f2, dense = TRUE)
  expect_length(dense2, 2L^3L - 1L)
  expect_true(all(c("A&C", "B&C", "A&B&C") %in% names(dense2)))
  expect_equal(dense2[["A&C"]], 0)
})
