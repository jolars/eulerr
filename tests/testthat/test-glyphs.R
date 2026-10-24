find_grob_class <- function(x, class) {
  if (inherits(x, class)) {
    return(x)
  }
  if (is.null(x$children)) {
    return(NULL)
  }
  for (child in x$children) {
    found <- find_grob_class(child, class)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

make_glyph_content <- function(plot) {
  grid::pushViewport(grid::viewport(
    xscale = plot$data$xlim,
    yscale = plot$data$ylim
  ))
  on.exit(grid::popViewport())
  grid::makeContent(find_grob_class(plot, "EulerGlyphs"))
}

test_that("unit glyphs are packed and rendered", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(fit, glyphs = TRUE)
  glyphs <- find_grob_class(plot, "EulerGlyphs")

  expect_s3_class(glyphs, "EulerGlyphs")
  expect_identical(glyphs$mode, "dots")
  expect_equal(glyphs$values, c(A = 3, B = 2, "A&B" = 1))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("new dot arrangements preserve counts and ignore the seed", {
  fit <- euler(c(A = 100), shape = "square")
  pdf(NULL)
  on.exit(grDevices::dev.off())

  for (arrangement in c("rectangular", "low_discrepancy")) {
    dots <- lapply(c(0, 42), function(seed) {
      plot <- plot(
        fit,
        labels = FALSE,
        glyphs = list(arrangement = arrangement, seed = seed)
      )
      make_glyph_content(plot)$children[[1L]]
    })
    expect_equal(dots[[1L]], dots[[2L]])
    expect_length(dots[[1L]]$x, 100L)
    expect_length(dots[[1L]]$y, 100L)

    if (arrangement == "rectangular") {
      xs <- sort(unique(round(as.numeric(dots[[1L]]$x), 9)))
      ys <- sort(unique(round(as.numeric(dots[[1L]]$y), 9)))
      expect_gt(length(xs), 1L)
      expect_gt(length(ys), 1L)
      expect_equal(diff(xs), rep(xs[2L] - xs[1L], length(xs) - 1L))
      expect_equal(diff(ys), rep(xs[2L] - xs[1L], length(ys) - 1L))
    }
  }
})

test_that("dot boundary spacing controls edges and obstacles independently", {
  fit <- euler(c(A = 100), shape = "square")
  local_mocked_bindings(
    glyph_obstacles = function(x) {
      list(h = 0, k = 0, width = 2, height = 2)
    }
  )
  pdf(NULL)
  on.exit(grDevices::dev.off())

  for (arrangement in c(
    "uniform",
    "rectangular",
    "low_discrepancy",
    "random"
  )) {
    plot <- plot(
      fit,
      labels = FALSE,
      glyphs = list(
        counts = c(A = 36),
        radius = 0.25,
        gap = 0.25,
        boundary_gap = 3,
        arrangement = arrangement,
        obstacle_policy = "strict"
      )
    )
    dots <- make_glyph_content(plot)$children[[1L]]
    x <- as.numeric(dots$x)
    y <- as.numeric(dots$y)
    expect_length(x, 36L)
    expect_equal(as.numeric(dots$r), 0.25)

    boundary_clearance <- pmin(5 - abs(x), 5 - abs(y))
    obstacle_clearance <- sqrt(pmax(abs(x) - 1, 0)^2 + pmax(abs(y) - 1, 0)^2)
    expect_gte(min(boundary_clearance), 1 - 1e-8)
    expect_gte(min(obstacle_clearance), 1 - 1e-8)
    expect_gte(min(stats::dist(cbind(x, y))), 0.625 - 1e-8)
  }
})

test_that("dot boundary spacing defaults to the gap between dots", {
  fit <- euler(c(A = 10), shape = "square")
  pdf(NULL)
  on.exit(grDevices::dev.off())

  dots <- lapply(list(NULL, 0, 1, 0L, 3, 3L), function(boundary_gap) {
    plot <- plot(
      fit,
      labels = FALSE,
      glyphs = list(gap = 1, boundary_gap = boundary_gap)
    )
    make_glyph_content(plot)$children
  })
  expect_equal(dots[[1L]], dots[[3L]])
  expect_equal(dots[[2L]], dots[[4L]])
  expect_equal(dots[[5L]], dots[[6L]])
  expect_false(isTRUE(all.equal(dots[[1L]], dots[[2L]])))
  expect_false(isTRUE(all.equal(dots[[1L]], dots[[5L]])))
})

test_that("dot boundary spacing must be finite and nonnegative", {
  fit <- euler(c(A = 1))
  for (gap in list(-1, NA_real_, Inf, c(0, 1), "wide")) {
    expect_error(
      plot(fit, glyphs = list(boundary_gap = gap)),
      "glyphs\\$boundary_gap"
    )
  }
})

test_that("new member arrangements preserve names and ignore the seed", {
  fit <- euler(c(A = 12), shape = "square")
  labels <- paste("Member", seq_len(12))
  pdf(NULL)
  on.exit(grDevices::dev.off())

  for (arrangement in c("rectangular", "low_discrepancy")) {
    members <- lapply(c(0, 42), function(seed) {
      plot <- plot(
        fit,
        labels = FALSE,
        glyphs = list(
          mode = "members",
          labels = list(A = labels),
          arrangement = arrangement,
          seed = seed
        )
      )
      make_glyph_content(plot)$children
    })
    expect_equal(members[[1L]], members[[2L]])
    expect_equal(unname(vapply(members[[1L]], `[[`, "", "label")), labels)
  }
})

test_that("member rows can be balanced without losing names or shrinking text", {
  fit <- euler(c(A = 400), shape = "square")
  labels <- rep("Member", 12)
  pdf(NULL)
  on.exit(grDevices::dev.off())

  members <- lapply(list(NULL, 1, 1L), function(ratio) {
    plot <- plot(
      fit,
      labels = FALSE,
      glyphs = list(
        mode = "members",
        labels = list(A = labels),
        target_aspect_ratio = ratio
      )
    )
    make_glyph_content(plot)$children
  })
  aspect_ratio <- function(children) {
    grid::pushViewport(grid::viewport(
      xscale = c(-10, 10),
      yscale = c(-10, 10)
    ))
    on.exit(grid::popViewport())
    widths <- vapply(
      children,
      function(child) {
        grid::convertWidth(grid::grobWidth(child), "native", TRUE)
      },
      0
    )
    heights <- vapply(
      children,
      function(child) {
        grid::convertHeight(grid::grobHeight(child), "native", TRUE)
      },
      0
    )
    h <- vapply(children, function(child) as.numeric(child$x), 0)
    k <- vapply(children, function(child) as.numeric(child$y), 0)
    diff(range(h - widths / 2, h + widths / 2)) /
      diff(range(k - heights / 2, k + heights / 2))
  }
  for (children in members) {
    expect_equal(unname(vapply(children, `[[`, "", "label")), labels)
  }
  expect_equal(members[[2L]], members[[3L]])
  expect_equal(
    vapply(members[[1L]], function(child) child$gp$fontsize, 0),
    vapply(members[[2L]], function(child) child$gp$fontsize, 0)
  )
  expect_lt(
    abs(log(aspect_ratio(members[[2L]]))),
    abs(log(aspect_ratio(members[[1L]])))
  )
})

test_that("member row aspect ratios must be finite and positive", {
  fit <- euler(c(A = 1))
  for (ratio in list(0, -1, NA_real_, Inf, c(1, 2), "square")) {
    expect_error(
      plot(
        fit,
        glyphs = list(
          mode = "members",
          labels = list(A = "Ada"),
          target_aspect_ratio = ratio
        )
      ),
      "glyphs\\$target_aspect_ratio"
    )
  }
})

test_that("strict obstacles report blocked dots and member labels", {
  fit <- euler(c(A = 1), shape = "square")
  local_mocked_bindings(
    glyph_obstacles = function(x) {
      list(h = 0, k = 0, width = 2, height = 2)
    }
  )
  pdf(NULL)
  on.exit(grDevices::dev.off())

  for (mode in c("dots", "members")) {
    for (arrangement in c(
      "uniform",
      "rectangular",
      "low_discrepancy",
      "random"
    )) {
      glyphs <- list(
        mode = mode,
        labels = list(A = "Ada"),
        arrangement = arrangement
      )
      best_effort <- plot(fit, labels = FALSE, glyphs = glyphs)
      expect_silent(content <- make_glyph_content(best_effort))
      expect_length(content$children, 1L)

      glyphs$obstacle_policy <- "strict"
      strict <- plot(fit, labels = FALSE, glyphs = glyphs)
      expect_warning(
        content <- make_glyph_content(strict),
        "1 .*could not be placed"
      )
      expect_length(content$children, 0L)
    }
  }
})

test_that("glyph obstacle policy can be set globally", {
  old <- eulerr_options(glyphs = list(obstacle_policy = "strict"))
  on.exit(eulerr_options(old))
  plot <- plot(euler(c(A = 1)), glyphs = TRUE)
  glyphs <- find_grob_class(plot, "EulerGlyphs")
  expect_identical(glyphs$options$obstacle_policy, "strict")
})

test_that("glyph counts must be nonnegative integers", {
  fit <- euler(c(A = 1.5, B = 2, "A&B" = 1))
  expect_error(plot(fit, glyphs = TRUE), "nonnegative integers")

  fit <- euler(c(A = 2, B = 2, "A&B" = 1))
  expect_error(
    plot(fit, glyphs = list(counts = c(A = -1))),
    "nonnegative integers"
  )
  expect_error(
    plot(fit, glyphs = list(counts = c(C = 1))),
    "Unknown set"
  )
  expect_error(
    plot(fit, glyphs = list(counts = c("A&B" = 1, "B&A" = 1))),
    "duplicate regions"
  )
})

test_that("glyph safety limits skip oversized layers", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  expect_warning(
    plot(fit, glyphs = list(max_items = 2)),
    "exceed `glyphs\\$max_items`"
  )
})

test_that("member labels are packed at draw time", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(
    fit,
    glyphs = list(
      mode = "members",
      labels = list(A = c("Ada", "Grace"), B = "Alan", "A&B" = "Kay")
    )
  )
  glyphs <- find_grob_class(plot, "EulerGlyphs")

  expect_identical(glyphs$mode, "members")
  expect_equal(unname(lengths(glyphs$values)), c(2L, 1L, 1L))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("outside set labels use their own placement layer", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  plot <- plot(
    fit,
    labels = list(position = "outside"),
    quantities = TRUE
  )
  set_labels <- find_grob_class(plot, "EulerSetLabels")

  expect_s3_class(set_labels, "EulerSetLabels")
  expect_true(all(is.na(plot$data$centers$labels)))
  expect_equal(plot$data$centers$quantities, c("3", "2", "1"))

  pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(grid::grid.draw(plot))
})

test_that("glyph controls are validated", {
  fit <- euler(c(A = 3, B = 2, "A&B" = 1))
  expect_error(plot(fit, glyphs = list(gap = -1)), "glyphs\\$gap")
  expect_error(
    plot(fit, glyphs = list(arrangement = "spiral")),
    "one of"
  )
  expect_error(
    plot(fit, glyphs = list(obstacle_policy = "ignore")),
    "one of"
  )
  expect_error(
    plot(fit, glyphs = list(mode = "members", labels = c(A = "Ada"))),
    "fully named list"
  )
  expect_error(
    plot(fit, labels = list(position = "outside", angular_steps = 4)),
    "at least 8"
  )
})
