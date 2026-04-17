# Plot Euler and Venn diagrams

Plot diagrams fit with
[`euler()`](https://jolars.github.io/eulerr/reference/euler.md) and
[`venn()`](https://jolars.github.io/eulerr/reference/venn.md) using
[`grid::Grid()`](https://rdrr.io/r/grid/Grid.html) graphics. This
function sets up all the necessary plot parameters and computes the
geometry of the diagram.
[`plot.eulergram()`](https://jolars.github.io/eulerr/reference/plot.eulergram.md),
meanwhile, does the actual plotting of the diagram. Please see the
**Details** section to learn about the individual settings for each
argument.

## Usage

``` r
# S3 method for class 'euler'
plot(
  x,
  fills = TRUE,
  patterns = FALSE,
  edges = TRUE,
  legend = FALSE,
  labels = identical(legend, FALSE),
  quantities = FALSE,
  strips = NULL,
  bg = FALSE,
  main = NULL,
  n = 200L,
  adjust_labels = TRUE,
  ...
)

# S3 method for class 'eulerr_venn'
plot(
  x,
  fills = TRUE,
  patterns = FALSE,
  edges = TRUE,
  legend = FALSE,
  labels = identical(legend, FALSE),
  quantities = TRUE,
  strips = NULL,
  bg = FALSE,
  main = NULL,
  n = 200L,
  adjust_labels = TRUE,
  ...
)

# S3 method for class 'venn'
plot(...)
```

## Arguments

- x:

  an object of class `'euler'`, generated from
  [`euler()`](https://jolars.github.io/eulerr/reference/euler.md)

- fills:

  a logical, vector, or list of graphical parameters for the fills in
  the diagram. Vectors are assumed to be colors for the fills. See
  [`grid::grid.path()`](https://rdrr.io/r/grid/grid.path.html).

- patterns:

  a logical, vector, or list of graphical parameters for fill patterns
  in the diagram. Vectors are assumed to be pattern types (currently
  `"stripes"` or `NA`), where `NA` means no pattern. Supported list
  items are `type`, `angle`, `col`, `lwd`, and `alpha`.

- edges:

  a logical, vector, or list of graphical parameters for the edges in
  the diagram. Vectors are assumed to be colors for the edges. See
  [`grid::grid.polyline()`](https://rdrr.io/r/grid/grid.lines.html).

- legend:

  a logical scalar or list. If a list, the item `side` can be used to
  set the location of the legend. See
  [`grid::grid.legend()`](https://rdrr.io/r/grid/legendGrob.html).

- labels:

  a logical, vector, or list. Vectors are assumed to be text for the
  labels. See
  [`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html).

- quantities:

  a logical, vector, or list. Vectors are assumed to be text for the
  quantities' labels, which by default are the original values in the
  input to
  [`euler()`](https://jolars.github.io/eulerr/reference/euler.md). In
  addition to plain vectors, `quantities$labels` can also be a named
  vector keyed by subset names (e.g., `"A"`, `"B"`, `"A&B"`), which is
  useful for supplying custom text for overlap regions. If
  `quantities$labels` is `NULL`, `quantities$format` can be used to
  control number formatting as a list with an item `fun` (a function
  such as [`signif()`](https://rdrr.io/r/base/Round.html) or
  [`round()`](https://rdrr.io/r/base/Round.html)) and optional extra
  arguments passed to that function (for example,
  `list(fun = prettyNum, big.mark = ",")`). `quantities$total` can be
  used to set an external denominator for percent/fraction quantities
  (instead of the plotted total). to arguments that apply to
  [`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html), an
  argument `type` may also be used which should be a combination of
  `"counts"`, `"percent"`, and `"fraction"`. The first item will be
  printed first and the second will be printed thereafter inside
  brackets. The default is `type = "counts"`.

- strips:

  a list, ignored unless the `'by'` argument was used in
  [`euler()`](https://jolars.github.io/eulerr/reference/euler.md)

- bg:

  a logical, character, or list controlling the background grob.
  Character values are interpreted as the background fill color.

- main:

  a title for the plot in the form of a character, expression, list or
  something that can be sensibly converted to a label via
  [`grDevices::as.graphicsAnnot()`](https://rdrr.io/r/grDevices/as.graphicsAnnot.html).
  A list of length one can be provided, in which case its only element
  is used as the label. If a list of longer length is provided, an item
  named `'label'` must be provided (and will be used for the actual
  text).

- n:

  number of vertices for the `edges` and `fills`

- adjust_labels:

  a logical. If `TRUE`, adjustment will be made to avoid overlaps or
  out-of-limits plotting of labels, quantities, and percentages.

- ...:

  parameters to update `fills` and `edges` with and thereby a shortcut
  to set these parameters
  [`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html).

## Value

Provides an object of class `'eulergram'` , which is a description of
the diagram to be drawn.
[`plot.eulergram()`](https://jolars.github.io/eulerr/reference/plot.eulergram.md)
does the actual drawing of the diagram.

## Details

The only difference between `plot.euler()` and `plot.venn()` is that
`quantities` is set to `TRUE` by default in the latter and `FALSE` in
the former.

Most of the arguments to this function accept either a logical, a
vector, or a list where

- logical values set the attribute on or off,

- vectors are shortcuts to commonly used options (see the individual
  parameters), and

- lists enable fine-grained control, including graphical parameters as
  described in [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) and
  control arguments that are specific to each argument.

The various [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) values
that are available for each argument are:

|            |       |       |        |            |        |        |      |
|------------|-------|-------|--------|------------|--------|--------|------|
|            | fills | edges | labels | quantities | strips | legend | main |
| col        |       | x     | x      | x          | x      | x      | x    |
| fill       | x     |       |        |            |        |        |      |
| alpha      | x     | x     | x      | x          | x      | x      | x    |
| lty        |       | x     |        |            |        |        |      |
| lwd        |       | x     |        |            |        |        |      |
| lex        |       | x     |        |            |        |        |      |
| fontsize   |       |       | x      | x          | x      | x      | x    |
| cex        |       |       | x      | x          | x      | x      | x    |
| fontfamily |       |       | x      | x          | x      | x      | x    |
| lineheight |       |       | x      | x          | x      | x      | x    |
| font       |       |       | x      | x          | x      | x      | x    |

Defaults for these values, as well as other parameters of the plots, can
be set globally using
[`eulerr_options()`](https://jolars.github.io/eulerr/reference/eulerr_options.md).

If the diagram has been fit using the `data.frame` or `matrix` methods
and using the `by` argument, the plot area will be split into panels for
each combination of the one to two factors.

For users who are looking to plot their diagram using another package,
all the necessary parameters can be collected if the result of this
function is assigned to a variable (rather than printed to screen).

## See also

[`euler()`](https://jolars.github.io/eulerr/reference/euler.md),
[`plot.eulergram()`](https://jolars.github.io/eulerr/reference/plot.eulergram.md),
[`grid::gpar()`](https://rdrr.io/r/grid/gpar.html),
[`grid::grid.polyline()`](https://rdrr.io/r/grid/grid.lines.html),
[`grid::grid.path()`](https://rdrr.io/r/grid/grid.path.html),
[`grid::grid.legend()`](https://rdrr.io/r/grid/legendGrob.html),
[`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html)

## Examples

``` r
fit <- euler(c("A" = 10, "B" = 5, "A&B" = 3))

# Customize colors, remove borders, bump alpha, color labels white
plot(fit,
     fills = list(fill = c("red", "steelblue4"), alpha = 0.5),
     labels = list(col = "white", font = 4))


# Add quantities to the plot
plot(fit, quantities = TRUE)


# Add a custom legend and retain quantities
plot(fit, quantities = TRUE, legend = list(labels = c("foo", "bar")))


# Plot without fills and distinguish sets with border types instead
plot(fit, fills = "transparent", lty = 1:2)


# Save plot parameters to plot using some other method
diagram_description <- plot(fit)

# Plots using 'by' argument
plot(euler(fruits[, 1:4], by = list(sex)), legend = TRUE)
```
