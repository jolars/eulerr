#' Venn diagrams
#'
#' This function fits Venn diagrams using an interface that is
#' almost identical to [euler()]. Strictly speaking,
#' Venn diagrams are Euler diagrams where every intersection is visible,
#' regardless of whether or not it is zero. In almost every incarnation of
#' Venn diagrams, however, the areas in the diagram are also
#' *non-proportional* to the input; this is also the case here.
#'
#' @inheritParams euler
#'
#' @return Returns an object of class `'venn', 'euler'` with items
#'   \item{ellipses}{a matrix of `h` and `k` (x and y-coordinates for the
#'     centers of the shapes), semiaxes `a` and `b`, and rotation angle `phi`}
#'   \item{original.values}{set relationships in the input}
#'   \item{fitted.values}{set relationships in the solution}
#'
#' @seealso [plot.venn()], [print.venn()], [euler()]
#'
#' @export
#'
#' @examples
#' # The trivial version
#' f1 <- venn(5, names = letters[1:5])
#' plot(f1)
#'
#' # Using data (a numeric vector)
#' f2 <- venn(c(A = 1, "B&C" = 3, "A&D" = 0.3))
venn <- function(combinations, ...)
{
  UseMethod("venn")
}

#' @describeIn venn a named numeric vector, with
#'   combinations separated by an ampersand, for instance `A&B = 10`.
#'   Missing combinations are treated as being 0.
#' @param names a character vector for the names of each set of the same
#'   length as 'combinations'. Must not be `NULL` if `combinations` is a
#'   one-length numeric.
#' @export
venn.default <- function(combinations,
                         input = c("disjoint", "union"),
                         names = letters[length(combinations)],
                         ...)
{
  if (is.numeric(combinations) && length(combinations) == 1) {
    if (is.null(names))
      stop("'names' must not be NULL when 'combinations' is a length ",
           "one integer.")

    if (combinations < 1)
      stop("'combinations' cannot be less than 1 when 'combinations' is a ",
           "length one integer.")

    if (combinations > 5)
      stop("'venn()' only supports diagrams with up to five sets.")

    if (length(names) != combinations)
      stop("'names' must be as long as 'combinations'.")

    n <- combinations
    setnames <- names
    id <- bit_indexr(n)

    combo_names <- unlist(lapply(apply(id, 1, function(x) names[x]),
                                 paste,
                                 collapse = "&"))
    combinations <- rep.int(1, nrow(id))
    names(combinations) <- combo_names
  } else {
    n_combinations <- length(unique(unlist(strsplit(names(combinations), "&"))))

    if (n_combinations > 5)
      stop("'venn()' only supports diagrams with up to five sets")
  }

  fit_diagram(combinations,
              type = "venn",
              input = input,
              shape = "ellipse",
              loss = "sse",
              control = list(),
              ...)
}

#' @describeIn venn A table with `max(dim(x)) < 3`.
#' @export
#'
#' @examples
#'
#' # The table method
#' venn(pain, factor_names = FALSE)
venn.table <- function(combinations, ...)
{
  x <- as.data.frame(combinations)
  venn(x[, !(names(x) == "Freq")], weights = x$Freq, ...)
}

#' @describeIn venn a `data.frame` of logicals, binary integers, or
#'   factors.
#' @export
#' @examples
#'
#' # Using grouping via the 'by' argument through the data.frame method
#' venn(fruits, by = list(sex, age))
#'
venn.data.frame <- function(combinations,
                            weights = NULL,
                            by = NULL,
                            sep = "_",
                            factor_names = TRUE,
                            ...)
{
  by <- substitute(by)
  facs <- eval(by, combinations)

  combinations <- parse_dataframe(combinations,
                                  weights,
                                  by,
                                  facs,
                                  sep,
                                  factor_names)

  if (is.list(combinations)) {
    out <- lapply(combinations, venn, ...)
    class(out) <- c("venn", "euler", "list")
    attr(out, "groups") <- attr(combinations, "groups")
  } else {
    out <- venn(combinations, ...)
  }

  out
}

#' @describeIn venn a matrix that can be converted to a data.frame of logicals
#'   (as in the description above) via [base::as.data.frame.matrix()].
#' @export
#' @inheritParams euler
#'
#' @examples
#'
#' # Using the matrix method
#' venn(organisms)
#'
#' # Using weights
#' venn(organisms, weights = c(10, 20, 5, 4, 8, 9, 2))
venn.matrix <- function(combinations, ...)
{
  venn(as.data.frame(combinations), ...)
}

#' @describeIn venn a list of vectors, each vector giving the contents of
#'   that set (with no duplicates). Vectors in the list do not need to be named.
#' @export
#' @inheritParams euler
#' @examples
#'
#' # A venn diagram from a list of sample spaces (the list method)
#' venn(plants[c("erigenia", "solanum", "cynodon")])
venn.list <- function(combinations, ...)
{
  venn(parse_list(combinations), input = "union", ...)
}
