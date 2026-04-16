# Print (plot) Euler diagram

This function is responsible for the actual drawing of `'eulergram'`
objects created through
[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
`print.eulergram()` is an alias for `plot.eulergram()`, which has been
provided so that
[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
gets called automatically.

## Usage

``` r
# S3 method for class 'eulergram'
plot(x, newpage = TRUE, ...)

# S3 method for class 'eulergram'
print(x, ...)
```

## Arguments

- x:

  an object of class `'eulergram'`, usually the output of
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)

- newpage:

  if `TRUE`, opens a new page via `grid.newpage()` to draw on

- ...:

  ignored

## Value

A plot is drawn on the current device using
[`grid::Grid()`](https://rdrr.io/r/grid/Grid.html) graphics.
