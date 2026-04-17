# Get or set global graphical parameters for eulerr

This function provides a means to set default parameters for functions
in eulerr. Query `eulerr_options()` (without any argument) to see all
the available options and read more about the plot-related ones in
[`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) and
[`graphics::par()`](https://rdrr.io/r/graphics/par.html).

## Usage

``` r
eulerr_options(...)
```

## Arguments

- ...:

  objects to update the global graphical parameters for eulerr with.

## Value

This function gets or sets updates in the global environment that are
used in
[`plot.euler()`](https://jolars.github.io/eulerr/reference/plot.euler.md).

## Details

Currently, the following items will be considered:

- pointsize:

  size in pts to be used as basis for fontsizes and some margin sizes in
  the resulting plot

- fills:

  a list of items `fill` and `alpha`

- patterns:

  a list of items `type`, `angle`, `col`, `lwd`, and `alpha`

- edges:

  a list of items `col`, `alpha`, `lex`, `lwd`, and `lty`

- labels:

  a list of items `rot`, `col`, `alpha`, `fontsize`, `cex`,
  `fontfamily`, `fontface`, `lineheight`, and `font`

- quantities:

  a list of items `type`, `format`, `total`, `rot`, `col`, `alpha`,
  `fontsize`, `cex`, `fontfamily`, `lineheight`, and `font`

- strips:

  `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `lineheight`, and
  `font`

- legend:

  arguments to
  [`grid::legendGrob()`](https://rdrr.io/r/grid/legendGrob.html) as well
  as `col`, `alpha`, `fontsize`, `cex`, `fontfamily`, `lineheight`, and
  `font`

- main:

  arguments to
  [`grid::textGrob()`](https://rdrr.io/r/grid/grid.text.html)

- padding:

  a [`grid::unit()`](https://rdrr.io/r/grid/unit.html) giving the
  padding between various elements in plots from
  [`plot.euler()`](https://jolars.github.io/eulerr/reference/plot.euler.md),
  which you can change if you, for instance, want to increase spacing
  between labels, quantities, and percentages.

## See also

[`plot.euler()`](https://jolars.github.io/eulerr/reference/plot.euler.md),
[`grid::gpar()`](https://rdrr.io/r/grid/gpar.html),
[`graphics::par()`](https://rdrr.io/r/graphics/par.html)

## Examples

``` r
eulerr_options(edges = list(col = "blue"), fontsize = 10)
eulerr_options(n_threads = 2)
```
