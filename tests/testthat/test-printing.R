context("Printing")

test_that("printing venn objects work", {
  f <- venn(3, names = letters[1:3])
  expect_silent(dont_print(f))
})
