#' Skyline packing algorithm
#'
#' @param m a matrix of vertices for the rectangles
#'
#' @return A matrix with updated vertices for the rectangles.
#'
#' @keywords internal
skyline_pack <- function(m) {
  # TODO: Add rotation to boxes as well.
  # TODO: Port to c++

  n <- NCOL(m)
  w <- m[2L, ] - m[1L, ]
  h <- m[4L, ] - m[3L, ]
  sizes <- h*w

  # Add some padding for the rectangles
  #padding <- 1.6*sqrt(sum(sizes))*0.015
  padding <- sum(w, na.rm = TRUE)*0.8*0.015

  # Pick a maximum bin width. Make sure the largest rectangle fits.
  #bin_w <- max(1.6*sqrt(sum(sizes)), w + padding)
  bin_w <- max(sum(w, na.rm = TRUE)*0.8,
               sum(w[order(w, decreasing = TRUE)][1:2] + 2*padding),
               w + padding)

  w <- w + padding
  h <- h + padding

  tol <- sqrt(.Machine$double.eps)

  m[] <- 0

  # Initialize the skyline
  skyline <- matrix(c(0, 0, bin_w, 0), ncol = 2)

  for (i in 1L:n) {
    # Order the points by y coordinate
    ord <- order(skyline[2L, ])

    # Start by examining the lowest rooftop on the skyline
    k <- 0L

    looking <- TRUE
    while (looking) {
      j <- which(ord == (1L + 2L*k) | ord == (2L + 2L*k))[1L]

      p1 <- ord[ord == j]
      p2 <- ord[ord == j + 1L]

      left  <- skyline[2L, 1L:p1] - skyline[2L, p1] > tol
      right <- skyline[2L, p2:NCOL(skyline)] - skyline[2L, p2] > tol

      if (any(left)) {
        # There is a taller rooftop on the skyline to the left
        next_left <- which(left)[sum(left)]
      } else {
        next_left <- 1L
      }

      if (any(right)) {
        # There is a taller rooftop on the skyline to the right
        next_right <- which(right)[1L]
      } else {
        next_right <- NCOL(skyline)
      }

      if (w[i] <= skyline[1L, next_right] - skyline[1L, next_left]) {
        # Build a new building on the skyline
        m[1L, i] <- skyline[1L, next_left]
        m[2L, i] <- skyline[1L, next_left] + w[i]
        m[3L, i] <- skyline[2L, p1]
        m[4L, i] <- skyline[2L, p2] + h[i]

        l <- if (next_left == 1) 0 else 1

        skyline[2L, next_left + l] <- skyline[2L, p1] + h[i]

        newcols <-
          rbind(c(skyline[1L, next_left] + w[i], skyline[1L, next_left] + w[i]),
                c(skyline[2L, p2] + h[i], skyline[2L, p2]))

        skyline <- cbind(skyline[, seq(1L, next_left + l), drop = FALSE],
                         newcols,
                         skyline[, seq(p2, NCOL(skyline)), drop = FALSE])

        # Check if there are any rooftops on the skyline beneath the new one
        underneath <- skyline[1L, ] > m[1L, i] & skyline[1L, ] < m[2L, i]

        if (any(underneath)) {
          # Drop down to the lowest level
          skyline[2L, which(underneath)[1L] - 1L] <-
            skyline[2L, which(underneath)[sum(underneath)]]
          skyline <- skyline[, !underneath, drop = FALSE]
        }

        looking <- FALSE
      } else {
        # Examine the next rooftop
        k <- k + 1L
      }
    }
  }
  m
}

#' Compress an Euler layout
#'
#' @param fpar an Euler layout fit with [euler()]
#' @param id the binary index of sets
#'
#' @return A modified fpar object.
#' @keywords internal
compress_layout <- function(fpar, id, fit) {
  # TODO: Port to c++
  n <- NCOL(id)

  clusters <- matrix(NA, nrow = n, ncol = n)

  for (i in 1:n) {
    for (j in 1:n) {
      clusters[i, j] <- any(id[, i] & id[, j] & fit > 0)
    }
  }

  for (i in 1:n) {
    for (j in 1:n) {
      if (any(clusters[i, ] & clusters[j, ])) {
        clusters[i, ] <- clusters[j, ] <- clusters[i, ] | clusters[j, ]
      }
    }
  }

  unique_clusters <- unique(lapply(split(clusters, row(clusters)), which))

  # Drop clusters that contain no elements (usually shapes without area)
  unique_clusters <- unique_clusters[lengths(unique_clusters) > 0L]
  n_clusters <- length(unique_clusters)

  if (n_clusters > 0) {
    bounds <- matrix(NA, ncol = n_clusters, nrow = 4L)

    for (i in seq_along(unique_clusters)) {
      ii <- unique_clusters[[i]]

      h <- fpar[ii, 1]
      k <- fpar[ii, 2]
      a <- fpar[ii, 3]
      b <- fpar[ii, 4]
      phi <- fpar[ii, 5]

      # normalize rotation by setting rotation angle between two first
      # ellipses to 0

      if (length(h) > 1) {
        theta <- atan2(k[2] - k[1], h[2] - h[1])

        h0 <- cos(-theta)*(h - h[1]) - sin(-theta)*(k - k[1]) + h[1]
        k0 <- sin(-theta)*(h - h[1]) + cos(-theta)*(k - k[1]) + k[1]
        phi0 <- phi - theta

        h <- h0
        k <- k0
        phi <- phi0

        xc <- mean(range(h))
        yc <- mean(range(k))

        # mirror across y axis if first shape is not at bottom
        if ((k[1] > yc)) {
          k <- yc - k
          phi <- -pi + phi
        }

        # mirror across x axis if first set is not furthest to the left
        if (h[1] > xc) {
          h <- xc -h
          phi <- -phi + pi
        }
      }

      limits <- get_bounding_box(h, k, a, b, phi)

      fpar[ii, 1] <- h
      fpar[ii, 2] <- k
      fpar[ii, 3] <- a
      fpar[ii, 4] <- b
      fpar[ii, 5] <- phi

      bounds[1:2, i] <- limits$xlim
      bounds[3:4, i] <- limits$ylim
    }

    if (n_clusters > 1) {
      # pack the bounding rectangles
      # TODO: Fix occasional errors in computing the bounding boxes.
      if (all(is.finite(bounds))) {
        new_bounds <- skyline_pack(bounds)
        for (i in seq_along(unique_clusters)) {
          ii <- unique_clusters[[i]]
          fpar[ii, 1] <- fpar[ii, 1] - (bounds[1, i] - new_bounds[1, i])
          fpar[ii, 2] <- fpar[ii, 2] - (bounds[3, i] - new_bounds[3, i])
        }
      }
    }
  }
  fpar
}

#' Center ellipses
#'
#' @param pars a matrix or data.frame of x coordinates, y coordinates, minor
#'   radius (a) and major radius (b)
#'
#' @return A centered version of `pars`.
#' @keywords internal
center_layout <- function(pars) {
  void <- pars[, 3L] == 0 | pars[, 4L] == 0
  h <- pars[!void, 1L]
  k <- pars[!void, 2L]
  a <- pars[!void, 3L]
  b <- pars[!void, 4L]
  phi <- pars[!void, 5L]

  cphi <- cos(phi)
  sphi <- sin(phi)
  xlim <- range(c(h + a*cphi, h + b*cphi, h - a*cphi, h - b*cphi))
  ylim <- range(c(k + a*sphi, k + b*sphi, k - a*sphi, k - b*sphi))

  pars[!void, 1L] <- h + abs(xlim[1L] - xlim[2L])/2 - xlim[2L]
  pars[!void, 2L] <- k + abs(ylim[1L] - ylim[2L])/2 - ylim[2L]
  if (any(!void)) {
    pars[void, 1L] <- sum(pars[!void, 1L])/sum(!void)
    pars[void, 2L] <- sum(pars[!void, 2L])/sum(!void)
  }
  pars
}
