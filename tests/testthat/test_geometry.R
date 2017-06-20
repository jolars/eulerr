context("Test geometry functions")

test_that("find_sets_containing_points function works", {
  res <- find_surrounding_sets(1:10, 1:10, 2, 2, 4)
  expect_equal(sum(res), 4)

  expect_equal(dist_point_circle(-4, -11, 4, -5, 7), 3)
})
