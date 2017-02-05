context("Test plot functions")

test_that("erroneous input to plot.euler return errors", {
  f <- euler(c(A = 5, B = 2))
  expect_error(p1 <- plot(f, fill_opacity = "f"))
  expect_error(p1 <- plot(f, fill_opacity = c(2, 3)))
})

test_that("normal plotting works without errors", {
  ff <- tempfile()
  png(filename = ff)
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_error(plot(
    f1,
    fill = c("black", "blue"),
    labels = c("Yes", "No"),
    fill_opacity = 0.3,
    fontface = 4
  ), NA)

  expect_error(plot(f1), NA)
  expect_error(plot(f1, key = TRUE, counts = TRUE), NA)
  expect_error(plot(f1, labels = c("asdf", "qwer")), NA)

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:2], by = dat[, 3:4])
  f3 <- euler(dat[, 1:2], by = dat[, 3])

  expect_error(plot(f2, fill = "transparent", lty = c(1, 2),
                    lwd = c(1, 2)), NA)
  expect_error(plot(f3), NA)
  dev.off()
  unlink(ff)
})

test_that("deprecated arguments throw warnings", {
  ff <- tempfile()
  png(filename = ff)
  f <- euler(c(A = 5, B = 2))
  expect_warning(plot(f, polygon_args = list(col = "black",
                                             border = "black",
                                             lty = 0)))
  expect_warning(plot(f, text_args = list(labels = c("a", "b"),
                                          cex = 2,
                                          font = 2)))

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:2], by = dat[, 3:4])

  expect_warning(plot(f2, mar = c(2, 2, 2, 2)))
  dev.off()
  unlink(ff)
})


