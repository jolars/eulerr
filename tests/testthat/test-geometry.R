test_that("bounding box computations work as expected", {
  f <- eulerr:::get_bounding_box(0, 0, 1, 1, 0)
  expect_equivalent(unlist(f), c(-1, 1, -1, 1))
  expect_equal(
    eulerr:::get_bounding_box(0, 1, 1),
    eulerr:::get_bounding_box(0, 1, 1, 1, pi)
  )

  expect_equal(
    eulerr:::get_bounding_box(0, 1, 1, 2, pi),
    eulerr:::get_bounding_box(0, 1, 1, 2, 2 * pi)
  )

  expect_equivalent(unlist(eulerr:::get_bounding_box(0, 0, 0)), rep(0, 4))

  expect_equal(
    eulerr:::get_bounding_box(0, 0, 1),
    eulerr:::get_bounding_box(c(0, 0), c(0, 0), c(1, 0.5))
  )
})

test_that("polygon_clip_rust delegates to eunoia for basic ops", {
  square_a <- list(x = c(0, 2, 2, 0), y = c(0, 0, 2, 2))
  square_b <- list(x = c(1, 3, 3, 1), y = c(1, 1, 3, 3))

  inter <- eulerr:::polygon_clip_rust(
    subject_x = square_a$x,
    subject_y = square_a$y,
    subject_id_lengths = length(square_a$x),
    clip_x = square_b$x,
    clip_y = square_b$y,
    op = "intersection"
  )
  expect_equal(length(inter$id_lengths), 1L)
  expect_equal(sum(inter$id_lengths), length(inter$x))

  empty <- eulerr:::polygon_clip_rust(
    subject_x = double(0),
    subject_y = double(0),
    subject_id_lengths = integer(0),
    clip_x = square_b$x,
    clip_y = square_b$y,
    op = "intersection"
  )
  expect_equal(length(empty$id_lengths), 0L)
})
