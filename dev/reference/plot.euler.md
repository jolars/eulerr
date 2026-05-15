# Plot Euler and Venn diagrams

Plot diagrams fit with
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md) and
[`venn()`](https://jolars.github.io/eulerr/dev/reference/venn.md) using
[`grid::Grid()`](https://rdrr.io/r/grid/Grid.html) graphics. This
function sets up all the necessary plot parameters and computes the
geometry of the diagram.
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md),
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
  annotations = NULL,
  strips = NULL,
  bg = FALSE,
  main = NULL,
  complement = TRUE,
  rotate = 0,
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
  complement = TRUE,
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
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)

- fills:

  a logical, vector, or list of graphical parameters for the fills in
  the diagram. Vectors are assumed to be colors for the fills. See
  [`grid::grid.path()`](https://rdrr.io/r/grid/grid.path.html). Named
  fill vectors can be matched in `fills$mode = "disjoint"` (default) or
  `fills$mode = "union"`.

- patterns:

  a logical, vector, or list of graphical parameters for fill patterns
  in the diagram. Vectors are assumed to be pattern types (currently
  `"stripes"` or `NA`), where `NA` means no pattern. Supported list
  items are `type`, `angle`, `col`, `lwd`, and `alpha`. Named pattern
  vectors can be matched in `patterns$mode = "disjoint"` (default) or
  `patterns$mode = "union"`.

- edges:

  a logical, vector, or list of graphical parameters for the edges in
  the diagram. Vectors are assumed to be colors for the edges. See
  [`grid::grid.polyline()`](https://rdrr.io/r/grid/grid.lines.html).

- legend:

  a logical scalar or list. If a list, the item `side` can be used to
  set the location of the legend and `symbol_size` can be used to scale
  the legend symbols independently of the text size. See
  [`grid::grid.legend()`](https://rdrr.io/r/grid/legendGrob.html).

- labels:

  a logical, vector, or list. Vectors are assumed to be text for the
  labels. See
  [`grid::grid.text()`](https://rdrr.io/r/grid/grid.text.html). In
  addition to the [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html)
  fields, the following placement controls are supported (delegated to
  the `eunoia` Rust crate): `labels$placement` (`"raycast"` (default) or
  `"force_directed"`) selects the exterior solver used when a label does
  not fit inside its region; `labels$margin` (numeric) overrides the
  per-region margin between an exterior label and the diagram (default
  is half the larger of the label's width and height);
  `labels$iterations` sets the iteration cap for the force-directed
  solver; `labels$tether` (`"poi"` (default) or `"boundary"`) chooses
  where the leader line attaches on the source region; `labels$gap`
  controls the visible gap between the leader tip and the label box edge
  — a bare numeric is interpreted as `lines` (font-relative), a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) is honoured as
  given, and the default `NULL` tracks `eulerr_options()$padding` so the
  gap matches the spacing between label and quantity; `labels$leader` is
  a list (`col`, `alpha`, `lwd`, `lty`, `lex`) styling the leader line
  drawn from the tether to the exterior label.

- quantities:

  a logical, vector, or list. Vectors are assumed to be text for the
  quantities' labels, which by default are the original values in the
  input to
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
  In addition to plain vectors, `quantities$labels` can also be a named
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
  brackets. The default is `type = "counts"`. For finer control over the
  rendered text, set `quantities$template` to a string with `{counts}`,
  `{percent}`, and/or `{fraction}` placeholders, for example
  `"{counts}\n{percent}"` to put the count and percentage on separate
  lines or `"n={counts} ({percent})"` for arbitrary layout. When
  `template` is set it overrides `type`; the set of placeholders in the
  template determines which values are computed.

- annotations:

  free-form per-region text rendered as a third stacked element below
  the quantity (or below the label when no quantity is drawn). Accepts a
  named character vector keyed by subset name (e.g.
  `c(A = "n = 12", "A&B" = "n = 3")`) as a shorthand for
  `list(labels = <vector>)`, or a list with `labels` plus
  [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) fields (`col`,
  `alpha`, `fontsize`, `cex`, `fontfamily`, `lineheight`, `font`,
  `rot`). Regions absent from `labels` are not annotated. The composite
  tag bbox grows to include the annotation, so exterior placement and
  leader lines adapt automatically. Defaults to slightly smaller text
  than `labels` / `quantities` (`cex = 0.8`).

- strips:

  a list, ignored unless the `'by'` argument was used in
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
  In addition to graphical parameters, this argument can include
  `labels = list(top = ..., left = ...)` for custom strip labels.
  Unnamed labels are interpreted in display order. Named labels are
  matched by factor levels and then reordered to display order.

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

- complement:

  a logical, character, or list controlling the container box and
  complement region for diagrams fit with `complement =` in
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).
  `TRUE` (default) draws the container with a dashed outline
  (`lty = 2`), no fill, and the complement count inside the complement
  region. `FALSE` suppresses the container and its label entirely. A
  character value is treated as a fill color shorthand. A list accepts
  `fill`, `alpha`, `col`, `lty`, `lwd`, `lex` (outline + label gpar),
  `fontsize`, `cex`, `font`, `fontfamily`, `lineheight` (label only),
  and `label` (custom text — defaults to the complement count). Also
  accepts the same placement controls as `labels` (`placement`,
  `margin`, `iterations`, `tether`, `gap`, `leader`) for the complement
  count label. Has no effect if the diagram was fit without
  `complement =`. Defaults can be set via
  `eulerr_options(complement = ...)`.

- rotate:

  a numeric value giving the angle in degrees by which to rotate the
  entire diagram layout. Positive values rotate counter-clockwise.
  Defaults to `0` (no rotation).

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
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
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

|            |       |       |        |            |             |        |        |      |
|------------|-------|-------|--------|------------|-------------|--------|--------|------|
|            | fills | edges | labels | quantities | annotations | strips | legend | main |
| col        |       | x     | x      | x          | x           | x      | x      | x    |
| fill       | x     |       |        |            |             |        |        |      |
| alpha      | x     | x     | x      | x          | x           | x      | x      | x    |
| lty        |       | x     |        |            |             |        |        |      |
| lwd        |       | x     |        |            |             |        |        |      |
| lex        |       | x     |        |            |             |        |        |      |
| fontsize   |       |       | x      | x          | x           | x      | x      | x    |
| cex        |       |       | x      | x          | x           | x      | x      | x    |
| fontfamily |       |       | x      | x          | x           | x      | x      | x    |
| lineheight |       |       | x      | x          | x           | x      | x      | x    |
| font       |       |       | x      | x          | x           | x      | x      | x    |

Defaults for these values, as well as other parameters of the plots, can
be set globally using
[`eulerr_options()`](https://jolars.github.io/eulerr/dev/reference/eulerr_options.md).

If the diagram has been fit using the `data.frame` or `matrix` methods
and using the `by` argument, the plot area will be split into panels for
each combination of the one to two factors. The `fills`, `patterns`,
`edges`, `labels`, `quantities`, and `annotations` arguments each accept
an optional `by_group` entry: a named list of override lists keyed by
panel name (the names of the fitted object). For multi-`by` fits the
panel name is the levels joined by `.`, e.g. `"Male.German"`. Panels not
listed in `by_group` use the top-level settings unchanged. Only
graphical fields (and `rot` for `labels`, `quantities`, and
`annotations`) may be overridden per panel; structural fields such as
`quantities$type`, `quantities$format`, `annotations$labels`, or
named-by-subset `fills$fill` must be set at the top level.

For users who are looking to plot their diagram using another package,
all the necessary parameters can be collected if the result of this
function is assigned to a variable (rather than printed to screen).

## See also

[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md),
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


# Add free-form per-region annotations below the counts
plot(
  fit,
  quantities = TRUE,
  annotations = c(A = "mean = 35", "A&B" = "mean = 41")
)


# Add a custom legend and retain quantities
plot(fit, quantities = TRUE, legend = list(labels = c("foo", "bar")))


# Plot without fills and distinguish sets with border types instead
plot(fit, fills = "transparent", lty = 1:2)


# Save plot parameters to plot using some other method
diagram_description <- plot(fit)

# Plots using 'by' argument
plot(euler(fruits[, 1:4], by = list(sex)), legend = TRUE)


# Per-panel styling with `by_group`
plot(
  venn(fruits[, 1:4], by = list(sex)),
  quantities = list(
    by_group = list(
      male = list(col = "steelblue"),
      female = list(col = "tomato")
    )
  )
)
```
