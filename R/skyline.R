skyline <- function(m) {
  hh <- sample(1:100, 1)
  set.seed(hh)
  m <- matrix(runif(20), nrow = 4)
  m <- apply(m, 2, sort)

  plot(0:1, 0:1, type = "n")
  #rect(m[1, ], m[3, ], m[2, ], m[4, ])

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
    # Order the points by y coordinate
    ord <- order(skyline[2, ])

    looking <- TRUE

    # Start by examining the lowest building on the skyline
    j <- 1
    k <- 2
    while (looking) {
      p1 <- ord[j]
      p2 <- ord[k]

      left  <- skyline[2, seq(1, p1)] - skyline[2, p1] > tol
      right <- skyline[2, seq(p2, ncol(skyline))] - skyline[2, p2] > tol

      if (any(left)) {
        # There is a taller rooftop on the skyline to the left
        next_left <- tail(which(left), 1)
      } else {
        next_left <- 1
      }

      if (any(right)) {
        # There is a taller rooftop on the skyline to the right
        next_right <- head(which(right), 1)
      } else {
        next_right <- NCOL(skyline)
      }

      if (w[i] <= diff(skyline[1, c(next_left, next_right)])) {
        # Fit inside the lowest building
        m[1, i] <- skyline[1, next_left]
        m[2, i] <- skyline[1, next_left] + w[i]
        m[3, i] <- skyline[2, p1]
        m[4, i] <- skyline[2, p2] + h[i]

        l <- ifelse(next_left == 1L, 0L, 1L)

        skyline[2, next_left + l] <- skyline[2, p1] + h[i]

        newcols <- rbind(
          c(skyline[1, next_left] + w[i], skyline[1,next_left] + w[i]),
          c(skyline[2, p2] + h[i]       , skyline[2, p2])
        )

        skyline <- cbind(skyline[, seq(1, next_left + l)],
                         newcols,
                         skyline[, seq(p2, NCOL(skyline))])

        # Check if there are any rooftops on the skyline beneath the new one
        underneath <- skyline[1, ] > m[1, i] & skyline[1, ] < m[2, i]

        if (any(underneath)) {
          skyline[2, which(underneath)[1] - 1] <- skyline[2, tail(which(underneath), 1)]
          skyline <- skyline[, !underneath]
        }


        looking <- FALSE
      } else {
        # Examine the next two lowest points
        j <- j + 2
        k <- k + 2
      }
    }
  }

  rect(m[1, ], m[3, ], m[2, ], m[4, ], border = 1:n)
  lines(t(skyline), col = "blue", lwd = 2)
}
