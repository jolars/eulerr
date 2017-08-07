skyline <- function(m) {
  set.seed(1)
  m <- matrix(runif(16), nrow = 4)
  m <- apply(m, 2, sort)

  plot(0:1, 0:1, type = "n")
  rect(m[1, ], m[3, ], m[2, ], m[4, ])

  n <- NCOL(m)
  w <- (m[2L, ] - m[1L, ])
  h <- (m[4L, ] - m[3L, ])
  sizes <- h*w

  # Pick a maximum bin width. Make sure the largest rectangle fits.
  #margin <- min(h, w) * 0.05
  bin_w <- max(1.3*sqrt(sum(sizes)), w )

  tol <- .Machine$double.eps

  m[] <- 0

  skyline <- cbind(c(0, 0), c(bin_w, 0))

  for (i in 1:n) {
    ord <- order(skyline[2, ])

    looking <- TRUE

    j <- 1
    k <- 2
    while (looking) {
      p1 <- ord[j]
      p2 <- ord[k]

      left <- which(skyline[2, seq(1, p1)] - skyline[2, c(p1, p2)] > tol)
      right <- which(skyline[2, seq(p1, ncol(skyline))] - skyline[2, c(p1, p2)] > tol)
      if (length(left) > 0 || length(right) > 0) {
        next_left <- tail(left, 1)
        next_right <- head(right, 1)
      }

      if (length(left) > 0) {
        next_left <- tail(left, 1)
      } else {
        next_left <- 1
      }

      if (length(right) > 0) {
        next_right <- head(right, 1)
      } else {
        next_right <- NCOL(skyline)
      }

      if (w[i] < diff(skyline[1, c(next_left, next_right)])) {
        # Fit inside the lowest trench
        m[1, i] <- skyline[1, next_left]
        m[2, i] <- skyline[1, next_left] + w[i]
        m[3, i] <- skyline[2, next_left]
        m[4, i] <- skyline[2, next_left] + h[i]

        skyline[2, next_left] <- skyline[2, next_left] + h[i]
        skyline <- cbind(skyline, m[c(2, 4), i], m[c(2, 3), i])

        # Reorder vertices in skyline
        skyline <- skyline[, order(skyline[1, ])]

        looking <- FALSE
      } else {
        # Examine the next two lowest points
        j <- j + 2
        k <- k + 2
      }
    }
  }

  lines(t(skyline))
}
