#' Skyline packing algorithm
#'
#' @param m A matrix of vertices for the rectangles.
#'
#' @return A matrix with updated vertices for the rectangles.
#'
#' @keywords internal
skyline_pack <- function(m) {
  # TODO: Add rotation to boxes as well.
  # TODO: Port to c++

  n <- NCOL(m)
  w <- (m[2L, ] - m[1L, ])
  h <- (m[4L, ] - m[3L, ])
  sizes <- h*w

  # Add some padding for the rectangles
  padding <- min(h, w) * 0.04

  # Pick a maximum bin width. Make sure the largest rectangle fits.
  bin_w <- max(1.4*sqrt(sum(sizes)), w + padding)

  w <- w + padding
  h <- h + padding

  tol <- sqrt(.Machine$double.eps)

  m[] <- 0

  # Initialize the skyline
  skyline <- cbind(c(0, 0), c(bin_w, 0))

  for (i in 1L:n) {
    # Order the points by y coordinate
    ord <- order(skyline[2, ])

    # Start by examining the lowest rooftop on the skyline
    j <- 1L
    k <- 2L

    looking <- TRUE
    while (looking) {
      p1 <- ord[j]
      p2 <- ord[k]

      left  <- skyline[2L, seq(1L, p1)] - skyline[2L, p1] > tol
      right <- skyline[2L, seq(p2, ncol(skyline))] - skyline[2L, p2] > tol

      if (any(left)) {
        # There is a taller rooftop on the skyline to the left
        next_left <- tail(which(left), 1L)
      } else {
        next_left <- 1L
      }

      if (any(right)) {
        # There is a taller rooftop on the skyline to the right
        next_right <- head(which(right), 1L)
      } else {
        next_right <- NCOL(skyline)
      }

      if (w[i] <= diff(skyline[1, c(next_left, next_right)])) {
        # Fit a new building in the skyline
        m[1L, i] <- skyline[1L, next_left]
        m[2L, i] <- skyline[1L, next_left] + w[i]
        m[3L, i] <- skyline[2L, p1]
        m[4L, i] <- skyline[2L, p2] + h[i]

        l <- ifelse(next_left == 1L, 0L, 1L)

        skyline[2L, next_left + l] <- skyline[2L, p1] + h[i]

        newcols <-
          rbind(c(skyline[1L, next_left] + w[i], skyline[1L, next_left] + w[i]),
                c(skyline[2L, p2] + h[i]       , skyline[2L, p2]))

        skyline <- cbind(skyline[, seq(1L, next_left + l)],
                         newcols,
                         skyline[, seq(p2, NCOL(skyline))])

        # Check if there are any rooftops on the skyline beneath the new one
        underneath <- skyline[1L, ] > m[1L, i] & skyline[1L, ] < m[2L, i]

        if (any(underneath)) {
          # Drop down to the lowest level
          skyline[2L, which(underneath)[1L] - 1L] <-
            skyline[2L, tail(which(underneath), 1L)]
          skyline <- skyline[, !underneath]
        }

        looking <- FALSE
      } else {
        # Examine the next rooftop
        j <- j + 2L
        k <- k + 2L
      }
    }
  }
  return(m)
}


#' Shelf packing algorithm for packing rectangles in a bin.
#'
#' @param m A matrix of vertices for the rectangles.
#'
#' @return A list with new vertices and the new order of the rectangles.
#'
#' @keywords internal
shelf_pack <- function(m) {
  # TODO(jlarsson): Introduce a better algorithm to do this, such as skyline.
  # TODO(jlarsson): Port to c++
  n <- ncol(m)
  w <- (m[2L, ] - m[1L, ])
  h <- (m[4L, ] - m[3L, ])
  sizes <- h*w

  # Pick a maximum bin width. Make sure the largest rectangle fits.

  margin <- min(h, w) * 0.05
  bin_w <- max(1.3*sqrt(sum(sizes)), w + margin)

  ord <- order(h, decreasing = TRUE)

  w <- w[ord] + margin
  h <- h[ord] + margin

  shelf_w <- rep.int(bin_w, n) # remaining shelf width
  shelf_h <- rep.int(0, n) # shelf heights

  x0 <- double(n)
  x1 <- double(n)
  y0 <- double(n)
  y1 <- double(n)

  hcurr <- h[1]

  for (i in seq_along(sizes)) {
    j <- 1L
    repeat {
      if (shelf_w[j] - w[i] < 0) {
        j <- j + 1L
        if (shelf_h[j] == 0) {
          shelf_h[j] <- shelf_h[j - 1L] + hcurr
          hcurr <- h[i]
        }
      } else {
        x0[i] <- bin_w - shelf_w[j]
        x1[i] <- x0[i] + w[i]
        y0[i] <- shelf_h[j]
        y1[i] <- y0[i] + h[i]

        shelf_w[j] <- shelf_w[j] - w[i]
        break;
      }
    }
  }
  return(list(xy = rbind(x0, x1, y0, y1), ord = ord))
}

#' Compress a Euler Layout
#'
#' @param fpar A Euler layout fit with [euler()]
#' @param id The binary index of sets.
#'
#' @return A modified fpar object.
#' @keywords internal
compress_layout <- function(fpar, id, fit) {
  # TODO: Port to c++
  n <- NCOL(id)

  clusters <- matrix(NA, n, n)

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

  bounds <- matrix(NA, ncol = length(unique_clusters), nrow = 4L)

  for (i in seq_along(unique_clusters)) {
    ii <- unique_clusters[[i]]
    h <- fpar[ii, 1L]
    k <- fpar[ii, 2L]
    if (NCOL(fpar) == 3L) {
      a <- b <- fpar[ii, 3L]
      phi <- 0
    } else {
      a <- fpar[ii, 3L]
      b <- fpar[ii, 4L]
      phi <- fpar[ii, 5L]
    }
    tx <- atan2(-b*tan(phi), a)
    ty <- atan2(b*tan(pi/2L - phi), a)

    bounds[, i] <-
      c(range(h + a*cos(tx)*cos(phi) - b*sin(tx)*sin(phi),
              h + a*cos(tx + pi)*cos(phi) - b*sin(tx + pi)*sin(phi)),
        range(k + b*sin(ty)*cos(phi) + a*cos(ty)*sin(phi),
              k + b*sin(ty + pi)*cos(phi) + a*cos(ty + pi)*sin(phi)))
  }

  new_bounds <- skyline_pack(bounds)

  for (i in seq_along(unique_clusters)) {
    ii <- unique_clusters[[i]]
    fpar[ii, 1L] = fpar[ii, 1L] - (bounds[1L, i] - new_bounds[1L, i])
    fpar[ii, 2L] = fpar[ii, 2L] - (bounds[3L, i] - new_bounds[3L, i])
  }

  fpar
}

#' Center Circles
#'
#' @param pars A matrix or data.frame of x coordinates, y coordinates, minor
#'   radius (a) and major radius (b).
#'
#' @return A centered version of `pars`.
#' @keywords internal
center_ellipses <- function(pars) {
  x <- pars[, 1L]
  y <- pars[, 2L]

  if (NCOL(pars) == 3) {
    # Circles
    a <- b <- pars[, 3L]
    phi <- 0
  } else {
    # Ellipses
    a <- pars[, 3L]
    b <- pars[, 4L]
    phi <- pars[,]
  }

  cphi <- cos(phi)
  sphi <- sin(phi)
  xlim <- range(c(x + a*cphi, x + b*cphi, x - a*cphi, x - b*cphi))
  ylim <- range(c(y + a*sphi, y + b*sphi, y - a*sphi, y - b*sphi))

  pars[, 1L] <- x + abs(xlim[1L] - xlim[2L]) / 2L - xlim[2L]
  pars[, 2L] <- y + abs(ylim[1L] - ylim[2L]) / 2L - ylim[2L]
  pars
}
