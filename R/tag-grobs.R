#' Setup grobs for labels (labels, quantities, percentages)
#'
#' @param data data for the locations of points and more
#' @param labels plot parameters for labels
#' @param number panel number, used for naming the resulting grob
#' @param quantities plot parameters for quantities
#'
#' @return A [grid::gTree()] object
#' @keywords internal
setup_tag <- function(data, labels, quantities, number) {

  x <- data$x
  y <- data$y

  label <- data$labels
  quantity <- data$quantities

  do_labels <- !is.null(labels) & !is.na(label)
  do_quantities <- !is.null(quantities) & !is.na(quantity)

  padding <- eulerr_options()$padding

  if (do_quantities) {
    quantities_grob <- textGrob(
      quantity,
      x = unit(x, "native"),
      y = unit(y, "native"),
      rot = quantities$rot[data$quantities_par_id],
      gp = quantities$gp[data$quantities_par_id],
      name = paste0("tag.quantity.", data$quantities_par_id)
    )
  } else {
    quantities_grob <- nullGrob()
  }

  if (do_labels) {
    labels_grob <- textGrob(
      label,
      x = unit(x, "native"),
      y = unit(y, "native"),
      rot = labels$rot[data$labels_par_id],
      gp = labels$gp[data$labels_par_id],
      name = paste0("tag.label.", data$labels_par_id)
    )

    if (do_quantities) {
      labels_grob$y <- labels_grob$y + 0.5*stringHeight(label) +
        0.5*grobHeight(quantities_grob) +
        padding
    }

  } else {
    labels_grob <- nullGrob()
  }

  grobs <- gList(
    label = labels_grob,
    quantity = quantities_grob
  )

  gTree(children = grobs,
        name = paste("tag", "number", number, sep = "."),
        cl = "EulerTag")
}

#' Avoid overlap for labels
#'
#' This method for [grid::makeContent()] sets up
#'
#' @export
#' @keywords internal
makeContent.EulerTags <- function(x) {

  xlim <- x$xlim
  ylim <- x$ylim

  # vp <- current.parent()
  #
  # x0 <- convertX(vp$x, "native", TRUE)
  # y0 <- convertY(vp$y, "native", TRUE)
  # w <- convertWidth(vp$width, "native", TRUE)
  # h <- convertHeight(vp$height, "native", TRUE)
  #
  # xlim <- c(x0 - w/2, x0 + w/2)
  # ylim <- c(y0 - h/2, y0 + h/2)

  if (!x$adjust_labels)
    return(x)

  d <- lapply(x$children, function(z) {
    lab <- z$children[[1]]

    if (inherits(lab, "null")) {
      out <- c(x = NA, y = NA, w = NA, h = NA)
    } else {
      c(x = convertX(lab$x, "native", TRUE),
        y = convertY(lab$y, "native", TRUE),
        w = convertWidth(grobWidth(lab), "native", TRUE),
        h = convertHeight(grobHeight(lab), "native", TRUE))
    }
  })

  d <- as.data.frame(do.call(rbind, d), stringsAsFactors = TRUE)

  nas <- is.na(d$x)

  d <- d[!nas, , drop = FALSE]

  dd <- lapply(x$children, function(z) {
    q <- z$children[[2]]

    if (inherits(q, "null")) {
      c(x = NA, y = NA, w = NA, h = NA)
    } else {
      c(x = convertX(q$x, "native", TRUE),
        y = convertY(q$y, "native", TRUE),
        w = convertWidth(grobWidth(q), "native", TRUE),
        h = convertHeight(grobHeight(q), "native", TRUE))
    }
  })

  dd <- as.data.frame(do.call(rbind, dd), stringsAsFactors = TRUE)

  dd <- dd[!is.na(dd$x), , drop = FALSE]

  padding <- eulerr_options()$padding
  padding <- convertHeight(padding, "native", TRUE)

  # treat quantities as points for the algorithm
  if (NROW(dd) > 0) {
    point_padding_x <- max(dd$w)*0.49
    point_padding_y <- max(dd$h)*0.49
    data_points <- cbind(dd$x, dd$y)
  } else {
    point_padding_x <- 0
    point_padding_y <- 0
    data_points <- cbind(d$x, d$y)
  }

  # only do something if there are any labels
  if (NROW(d) > 0) {
    # boxes are the labels (in eulerr terminology)
    boxes <- data.frame(x1 = d$x - 0.5*d$w - padding,
                        y1 = d$y - 0.5*d$h - padding,
                        x2 = d$x + 0.5*d$w + padding,
                        y2 = d$y + 0.5*d$h + padding,
                        stringsAsFactors = TRUE)

    repel <- repel_boxes(
      data_points = data_points,
      point_padding_x = point_padding_x,
      point_padding_y = point_padding_y,
      boxes = as.matrix(boxes),
      xlim = xlim,
      ylim = ylim,
      hjust = 0.5,
      vjust = 0.5,
      force_push = 1e-4,
      force_pull = 1e-4,
      maxiter = 1e5,
      direction = "both"
    )

    k <- 1

    for (i in seq_along(x$children)) {
      if (!inherits(x$children[[i]]$children[[1]], "null")) {
        x$children[[i]]$children[[1]]$x <- unit(repel[k, 1], "native")
        x$children[[i]]$children[[1]]$y <- unit(repel[k, 2], "native")
        k <- k + 1
      }
    }
  }

  x
}
