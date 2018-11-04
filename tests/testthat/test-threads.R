context("multi-thread processing")

test_that("using multiple threads returns equivalent output", {
  s <- c("A" = 30,
         "A&B" = 3, "A&C" = 3, "A&D" = 3,
         "A&B&C" = 2, "A&B&D" = 2, "A&C&D" = 2,
         "A&B&C&D" = 1)
  set.seed(1)
  f1 <- euler(s, n_threads = 1)
  set.seed(1)
  f2 <- euler(s, n_threads = 2)

  expect_equal(f1, f2)
})
