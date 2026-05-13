# Compose Euler Diagrams

Arrange two `eulergram` objects side-by-side or stacked, building up
multi-panel layouts with operator syntax. Compositions can be nested
arbitrarily, e.g. `(p1 | p2) / p3`.

## Usage

``` r
# S3 method for class 'eulergram'
e1 | e2

# S3 method for class 'eulergram'
e1/e2
```

## Arguments

- e1, e2:

  `eulergram` objects, typically returned by
  [`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md).

## Value

An `eulergram` containing the composed layout.

## Details

`|` arranges the two plots horizontally; `/` stacks them vertically. The
result is itself an `eulergram`, so further composition chains
naturally.

The gap between adjacent plots is controlled by the
`composition$spacing` entry of
[`eulerr_options()`](https://jolars.github.io/eulerr/dev/reference/eulerr_options.md),
which must be a [`grid::unit()`](https://rdrr.io/r/grid/unit.html) and
defaults to `grid::unit(1, "lines")`.

Because composition is binary and recursive, panels at different nesting
levels are not size-aligned. In `(p1 | p2) / p3`, `p3` spans the full
bottom row while `p1` and `p2` split the top row equally.

## See also

[`plot.euler()`](https://jolars.github.io/eulerr/dev/reference/plot.euler.md),
[`eulerr_options()`](https://jolars.github.io/eulerr/dev/reference/eulerr_options.md)

## Examples

``` r
p1 <- plot(euler(c(A = 1, B = 8, "A&B" = 1)))
p2 <- plot(euler(c(A = 1, C = 1, "A&C" = 1)))


p1 | p2

p1 / p2

p3 <- plot(euler(c(X = 3, Y = 2, "X&Y" = 1)))

(p1 | p2) / p3

```
