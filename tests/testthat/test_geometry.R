context("Test geometry functions")

test_that("that find_sets_containing_points function works", {
  x_int <- cbind(1:10, 1:10)
  res <- find_sets_containing_points(x_int, 2, 2, 4)
  expect_equal(sum(res), 4)

  expect_equal(dist_point_circle(-4, -11, 4, -5, 7), 3)
})
