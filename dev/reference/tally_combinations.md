# Tally set relationships

Tally set relationships

## Usage

``` r
tally_combinations(sets, weights)
```

## Arguments

- sets:

  a data.frame with set relationships and weights

- weights:

  a numeric vector

## Value

Calls
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)
after the set relationships have been coerced to a named numeric vector.
