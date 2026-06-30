# Fitted values of euler object

Fitted values of euler object

## Usage

``` r
# S3 method for class 'euler'
fitted(object, dense = FALSE, ...)
```

## Arguments

- object:

  object of class `'euler'`

- dense:

  if `TRUE`, return a vector covering every combination of the non-empty
  sets (`2^n - 1` entries), filling absent combinations with 0. The
  default (`FALSE`) returns the sparse vector stored on the object,
  which only contains entries that were either requested as input or fit
  to a non-zero area. Use `dense = TRUE` only for diagrams with few
  sets. The full enumeration is exponential in the number of non-empty
  sets.

- ...:

  ignored

## Value

A named numeric vector of fitted areas keyed by combination label (set
names joined by `&`).
