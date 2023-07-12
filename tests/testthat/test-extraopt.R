test_that("extra optimization works", {
  set.seed(1238)

  s <- c(
    "A" = 10,
    "B" = 10,
    "C" = 10,
    "A&B" = 4,
    "A&C" = 4,
    "B&C" = 4,
    "A&B&C" = 2
  )

  x <- euler(
    s,
    shape = "ellipse",
    control = list(
      extraopt_control = list(itermax = 200),
      extraopt_threshold = 0
    )
  )

  expect_is(x, "euler")
  y <- expect_error(dont_print(x), NA)
})
