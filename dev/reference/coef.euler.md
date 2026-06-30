# Return the fitted shape parameters from the euler object

Returns the `$shapes` data frame: a tagged uniform schema with one row
per set, a `type` column, and shape-specific columns
(`h, k, a, b, phi, width, height, side`) populated according to the
chosen shape (other columns are `NA`). For circle/ellipse fits the
legacy `$ellipses` slot is still populated for back-compat; consumers
that need the new shapes (rectangle, square) must read `$shapes`.

## Usage

``` r
# S3 method for class 'euler'
coef(object, ...)
```

## Arguments

- object:

  object of class `'euler'`

- ...:

  ignored

## Value

a data frame of the fitted shape parameters
