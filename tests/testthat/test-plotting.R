test_that("normal plotting works without errors", {
  tmp <- tempfile()
  png(tmp)

  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  expect_silent(plot(
    f1,
    fills = list(fill = c("black", "blue"), alpha = 0.3),
    labels = list(font = 4)
  ))

  expect_silent(plot(f1))
  expect_silent(plot(f1, legend = TRUE, quantities = TRUE))
  expect_silent(plot(f1, legend = FALSE, quantities = TRUE))
  expect_silent(plot(f1, legend = TRUE, labels = FALSE))
  expect_silent(plot(f1, labels = c("asdf", "qwer")))
  expect_silent(plot(f1, labels = str2expression(c("A", "B"))))
  expect_silent(plot(f1, labels = list(labels = str2expression(c("A", "B")))))
  expect_silent(plot(f1, main = "Hello"))
  expect_silent(plot(f1, expression = "phi[1]"))
  expect_silent(plot(f1, edges = c("white", "blue")))
  expect_silent(plot(f1, bg = "grey95"))
  expect_silent(plot(f1, bg = list(fill = "grey95", alpha = 0.8)))
  expect_silent(plot(f1, quantities = list(type = "percent")))
  expect_silent(plot(f1, quantities = list(type = "counts")))
  expect_silent(plot(f1, quantities = list(type = c("percent", "counts"))))
  expect_silent(plot(f1, quantities = list(type = c("counts", "percent"))))
  expect_silent(plot(
    f1,
    quantities = list(labels = c(A = "foo", B = "bar", "A&B" = "baz"))
  ))

  expect_error(plot(f1, quantities = list(type = c("asdf"))))

  grid <- expand.grid(
    labels = c(TRUE, FALSE),
    quantities = c(TRUE, FALSE),
    legend = c(TRUE, FALSE),
    fills = c(TRUE, FALSE),
    edges = c(TRUE, FALSE)
  )

  for (i in seq_len(nrow(grid))) {
    expect_silent(plot(
      f1,
      labels = grid$labels[!!i],
      quantities = grid$quantities[!!i],
      legend = grid$legend[!!i],
      fills = grid$fills[!!i],
      edges = grid$edges[!!i]
    ))
  }

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:3], by = list(Gender))
  f3 <- euler(dat, by = list(Gender, Nation))

  expect_silent(plot(
    f2,
    edges = list(fill = "transparent", lty = c(1, 2), lwd = c(1, 2))
  ))

  grid <- expand.grid(
    side = c(NA, "right", "left", "top", "bottom"),
    main = c("Title", FALSE),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(grid))) {
    if (is.na(grid$side[i])) {
      legend <- FALSE
    } else {
      legend <- list(side = grid$side[i])
    }

    expect_silent(plot(f1, legend = legend, main = grid$main[!!i]))
    expect_silent(plot(f2, legend = legend, main = grid$main[!!i]))
    expect_silent(plot(f3, legend = legend, main = grid$main[!!i]))
  }

  f4 <- euler(c(A = 1))
  expect_silent(plot(f4))

  dev.off()
  unlink(tmp)
})

test_that("plotting zero-fits works", {
  tmp <- tempfile()
  png(tmp)

  s <- c(a = 0, b = 0)
  expect_is(plot(euler(s)), "gTree")
  dev.off()
  unlink(tmp)
})

test_that("quantity formatters can be customized", {
  tmp <- tempfile()
  png(tmp)

  f <- euler(c(A = 10, B = 8, "A&B" = 3), input = "disjoint")

  p <- plot(
    f,
    quantities = list(
      type = "percent",
      format = list(fun = round, digits = 1)
    )
  )
  q <- p$data$centers$quantities
  names(q) <- rownames(p$data$centers)
  expect_equal(unname(q[c("A", "B", "A&B")]), c("47.6 %", "38.1 %", "14.3 %"))

  p <- plot(
    f,
    quantities = list(
      type = "counts",
      format = list(fun = round, digits = -1)
    )
  )
  q <- p$data$centers$quantities
  names(q) <- rownames(p$data$centers)
  expect_equal(unname(q[c("A", "B", "A&B")]), c("10", "10", "0"))

  p <- plot(
    f,
    quantities = list(
      type = "percent",
      total = 100,
      format = list(fun = round, digits = 1)
    )
  )
  q <- p$data$centers$quantities
  names(q) <- rownames(p$data$centers)
  expect_equal(unname(q[c("A", "B", "A&B")]), c("10 %", "8 %", "3 %"))

  p <- plot(
    f,
    quantities = list(
      type = "fraction",
      total = 100,
      format = list(fun = round, digits = 2)
    )
  )
  q <- p$data$centers$quantities
  names(q) <- rownames(p$data$centers)
  expect_equal(unname(q[c("A", "B", "A&B")]), c("0.1", "0.08", "0.03"))

  p <- plot(
    f,
    quantities = list(
      type = c("counts", "fraction"),
      total = 100,
      format = list(fun = round, digits = 2)
    )
  )
  q <- p$data$centers$quantities
  names(q) <- rownames(p$data$centers)
  expect_equal(unname(q[c("A", "B", "A&B")]), c("10 (0.1)", "8 (0.08)", "3 (0.03)"))

  expect_error(plot(f, quantities = list(format = 1)))
  expect_error(plot(f, quantities = list(format = list(fun = 1))))
  expect_error(plot(f, quantities = list(total = 0)))

  dev.off()
  unlink(tmp)
})

test_that("error_plot functions normally", {
  tmp <- tempfile()
  png(tmp)

  f <- euler(organisms)

  expect_silent(error_plot(f))
  expect_silent(error_plot(
    f,
    pal = grDevices::colorRampPalette(c("red", "blue"))
  ))
  expect_silent(error_plot(f, quantities = FALSE))
  expect_silent(error_plot(f, edges = FALSE))

  dev.off()
  unlink(tmp)
})

test_that("plots with euler lists works", {
  tmp <- tempfile()
  png(tmp)

  f1 <- euler(fruits[, 1:5], by = age)
  f2 <- euler(fruits[, 1:5], by = list(age, sex))

  expect_silent(plot(f1, legend = TRUE, strips = FALSE))
  expect_silent(plot(f2, strips = list(cex = 2, fontface = "bold")))
  expect_silent(plot(f1))

  dev.off()
  unlink(tmp)
})

test_that("venn plotting prefers eulerr class dispatch", {
  tmp <- tempfile()
  png(tmp)

  f <- venn(c(A = 10, B = 5, "A&B" = 2))
  expect_is(f, "eulerr_venn")
  expect_silent(plot(f))

  dev.off()
  unlink(tmp)
})

test_that("legacy plot.venn warns", {
  tmp <- tempfile()
  png(tmp)

  f <- venn(c(A = 10, B = 5, "A&B" = 2))
  expect_warning(plot.venn(f), "deprecated")

  dev.off()
  unlink(tmp)
})
