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

  done <- FALSE
  shelves <- matrix(NA, nrow = n, ncol = n)
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

  new_bounds <- shelf_pack(bounds)
  bounds <- bounds[, new_bounds$ord, drop = FALSE]
  unique_clusters <- unique_clusters[new_bounds$ord]

  for (i in seq_along(unique_clusters)) {
    ii <- unique_clusters[[i]]
    fpar[ii, 1L] = fpar[ii, 1L] - (bounds[1L, i] - new_bounds$xy[1L, i])
    fpar[ii, 2L] = fpar[ii, 2L] - (bounds[3L, i] - new_bounds$xy[3L, i])
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
