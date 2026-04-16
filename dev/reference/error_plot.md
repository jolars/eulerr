# Error plot for `euler` objects

This is a diagnostic tool for evaluating the fit from a call to
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
visually. A color key is provided by default, which represents the
chosen error metric so that one can easily detect which areas in the
diagram to be skeptical about.

## Usage

``` r
error_plot(
  x,
  type = c("regionError", "residuals"),
  quantities = TRUE,
  pal = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `euler`, typically the result of a call to
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md).

- type:

  error metric. `'regionError'` is the difference in *percentage points*
  from the input

- quantities:

  whether to draw the error metric on the plot

- pal:

  color palette for the fills in the legend

- ...:

  arguments passed down to
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
  Currently, providing `fills`, `legend`, or `strips` are not allowed
  and will return a warning.

## Value

Returns an object of class `eulergram`, which will be plotted on the
device in the same manner as objects from
[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).
See
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
for details.

## Details

Notice that this function is purely provided for diagnostic reasons and
does not come with the same kind of customization that
[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md)
provides: the color legend can only be customized in regards to its
color palette and another key (instead of labels) is completely turned
off.

## See also

[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md),
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)

## Examples

``` r
error_plot(euler(organisms), quantities = FALSE)
```
