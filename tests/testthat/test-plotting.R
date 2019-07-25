context("Plotting")

test_that("normal plotting works without errors", {
  f1 <- euler(c("A" = 10, "B" = 5, "A&B" = 2))

  p <- list()

  expect_silent(p[[1]] <- plot(f1,
                               fills = list(fill = c("black", "blue"),
                                            alpha = 0.3),
                               labels = list(font = 4)))

  expect_silent(p[[2]] <- plot(f1))
  expect_silent(p[[3]] <- plot(f1, legend = TRUE, quantities = TRUE))
  expect_silent(p[[4]] <- plot(f1, legend = FALSE, quantities = TRUE))
  expect_silent(p[[5]] <- plot(f1, legend = TRUE, labels = FALSE))
  expect_silent(p[[6]] <- plot(f1, labels = c("asdf", "qwer")))
  expect_silent(p[[7]] <- plot(f1, main = "Hello"))
  expect_silent(p[[8]] <- plot(f1, expression = "phi[1]"))
  expect_silent(p[[9]] <- plot(f1, edges = c("white", "blue")))
  expect_silent(p[[10]] <- plot(f1, quantities = list(type = "percent")))
  expect_silent(p[[11]] <- plot(f1, quantities = list(type = "counts")))
  expect_silent(p[[11]] <- plot(f1, quantities = list(type = c("percent",
                                                               "counts"))))

  expect_silent(p[[12]] <- plot(f1, quantities = list(type = c("counts",
                                                               "percent"))))

  expect_error(plot(f1, quantities = list(type = c("asdf"))))

  grid <- expand.grid(labels = c(TRUE, FALSE),
                      quantities = c(TRUE, FALSE),
                      legend = c(TRUE, FALSE),
                      fills = c(TRUE, FALSE),
                      edges = c(TRUE, FALSE))

  for (i in seq_len(nrow(grid))) {
    expect_silent(dont_plot(plot(f1,
                                 labels = grid$labels[!!i],
                                 quantities = grid$quantities[!!i],
                                 legend = grid$legend[!!i],
                                 fills = grid$fills[!!i],
                                 edges = grid$edges[!!i])))
  }

  dat <- data.frame(
    Liberal = sample(c(TRUE, FALSE), size = 100, replace = TRUE),
    Conservative = sample(c(TRUE, TRUE, FALSE), size = 100, replace = TRUE),
    Gender = sample(c("Men", "Women"), size = 100, replace = TRUE),
    Nation = sample(c("Sweden", "Denmark"), size = 100, replace = TRUE)
  )

  f2 <- euler(dat[, 1:3], by = list(Gender))
  f3 <- euler(dat, by = list(Gender, Nation))

  expect_silent(p[[10]] <- plot(f2,
                                fills = list(fill = "transparent",
                                             lty = c(1, 2),
                                             lwd = c(1, 2))))

  grid <- expand.grid(side = c(NA, "right", "left", "top", "bottom"),
                      main = c("Title", FALSE),
                      stringsAsFactors = FALSE)

  for (i in seq_len(nrow(grid))) {
    if (is.na(grid$side[i])) {
      legend <- FALSE
    } else {
      legend <- list(side = grid$side[i])
    }

    expect_silent(dont_plot(plot(f1, legend = legend, main = grid$main[!!i])))
    expect_silent(dont_plot(plot(f2, legend = legend, main = grid$main[!!i])))
    expect_silent(dont_plot(plot(f3, legend = legend, main = grid$main[!!i])))
  }

  f4 <- euler(c(A = 1))
  expect_silent(p[[11]] <- plot(f4))

  for (i in seq_along(p))
    expect_silent(dont_plot(p[[!!i]]))
})

test_that("plotting zero-fits works", {
  s <- c(a = 0, b = 0)
  expect_is(plot(euler(s)), "gTree")
})

test_that("error_plot functions normally", {
  f <- euler(organisms)

  expect_silent(dont_plot(error_plot(f)))
  expect_silent(dont_plot(error_plot(f,
                                     pal = grDevices::colorRampPalette(c(
                                       "red", "blue"
                                     )))))
  expect_silent(dont_plot(error_plot(f, quantities = FALSE)))
  expect_silent(dont_plot(error_plot(f, edges = FALSE)))
})

test_that("plots with euler lists works", {
  f1 <- euler(fruits[, 1:5], by = age)
  f2 <- euler(fruits[, 1:5], by = list(age, sex))

  expect_silent(dont_plot(plot(f1, legend = TRUE, strips = FALSE)))
  expect_silent(dont_plot(plot(f2, strips = list(cex = 2, fontface = "bold"))))
  expect_silent(dont_plot(plot(f1)))
})

test_that("label repelling functions", {
  f1 <- euler(c("very long label that has lots of words in it" = 1,
                "another long, long label that is sure to overlap" = 1,
                "very long label that has lots of words in it&another long, long label that is sure to overlap" = 10))
  expect_silent(dont_plot(plot(f1, adjust_labels = TRUE, quantities = TRUE)))
})
