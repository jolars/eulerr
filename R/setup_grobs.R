#' Grobify Euler objects
#'
#' @param x geometry data
#' @param fills fills params
#' @param edges edges params
#' @param labels labels params
#' @param quantities quantities params
#' @param number current diagram number
#' @param merged_sets sets that are the same and have been merged
#'
#' @return A [grid::gList()] is returned.
#' @keywords internal
setup_grobs <- function(x,
                        fills,
                        edges,
                        labels,
                        quantities,
                        number,
                        merged_sets) {
  data_edges <- x$edges
  data_fills <- x$fills
  data_tags <- x$centers
  fitted <- x$fitted.values
  empty_sets <- x$empty_sets
  empty_subsets <- x$empty_subsets

  do_tags <- !is.null(data_tags)
  do_edges <- !is.null(data_edges)
  do_fills <- !is.null(data_fills)
  do_labels <- !is.null(labels)
  do_quantities <- !is.null(quantities)

  xlim <- x$xlim
  ylim <- x$ylim

  n_e <- NROW(x$ellipses)
  n_id <- 2L^n_e - 1L
  id <- bit_indexr(n_e)

  #edges
  if (do_edges) {
    # edges
    if (is.null(data_edges$x)) {
      edges_grob <- grid::nullGrob()
    } else {
      edges_grob <- grid::polylineGrob(data_edges$x,
                                       data_edges$y,
                                       id.lengths = data_edges$id.lengths,
                                       default.units = "native",
                                       name = "edges.grob",
                                       gp = edges$gp[which(!empty_sets & !merged_sets)])
    }
  }

  # fills
  if (do_fills) {
    if (n_e == 0) {
      fills_grob <- grid::nullGrob()
    } else if (n_e == 1) {
      fills_grob <- grid::gList(grid::polygonGrob(
        data_fills[[1]]$x,
        data_fills[[1]]$y,
        default.units = "native",
        name = "fills.grob",
        gp = fills$gp[which(!empty_subsets)[1L]]
      ))
    } else {
      fills_grob <- vector("list", n_id)
      fill_id <- seq_len(n_id)
      empty <- !nonzero_fit(fitted)
      empty_cols <- colSums(id & !empty) == 0 | merged_sets
      # skip <- rowSums(id[, empty_cols, drop = FALSE]) > 0
      for (i in seq_len(n_e)) {
        if (empty[i] && !empty_cols[i]) {
          idx <- id[i, ]
          n_idx <- sum(idx)
          sub_id <- rowSums(id[, idx, drop = FALSE]) == n_idx
          next_num <- min(rowSums(id[sub_id & rowSums(id) > n_idx & !empty, ,
                                     drop = FALSE]))
          next_level <- rowSums(id) == next_num & sub_id
          if (any(next_level)) {
            fill_id[next_level] <- fill_id[i]
          }
        }
      }

      for (i in seq_len(n_id)) {
        if (is.null(data_fills[[i]])) {
          fills_grob[[i]] <- grid::nullGrob(name = paste0("fills.grob.", i))
        } else
          fills_grob[[i]] <- grid::pathGrob(
            data_fills[[i]]$x,
            data_fills[[i]]$y,
            id.lengths = data_fills[[i]]$id.lengths,
            default.units = "native",
            name = paste0("fills.grob.", i),
            gp = fills$gp[which(!empty_subsets)][fill_id[i]]
          )
      }
      fills_grob <- do.call(grid::gList, fills_grob)
    }
  }

  do_tags <- do_quantities || do_labels

  # labels
  if (do_tags) {
    tag_grobs <- gList()

    for (i in seq_len(NROW(data_tags))) {
      tag_grobs[[i]] <- setup_tag(data_tags[i, ],
                                  labels,
                                  quantities,
                                  number = i)
    }

    tags_gtree <- gTree(xlim = xlim,
                        ylim = ylim,
                        children = tag_grobs,
                        name = paste("tags"),
                        cl = "EulerTags")
  }

  grid::grobTree(if (do_fills) fills_grob,
                 if (do_edges) edges_grob,
                 if (do_tags) tags_gtree,
                 name = paste0("diagram.grob.", number))
}
