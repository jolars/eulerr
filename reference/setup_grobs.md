# Grobify Euler objects

Grobify Euler objects

## Usage

``` r
setup_grobs(
  x,
  fills,
  patterns,
  edges,
  labels,
  quantities,
  annotations = NULL,
  complement = NULL,
  number,
  merged_sets,
  n_vertices = 200L,
  placement_opts = NULL
)
```

## Arguments

- x:

  geometry data

- fills:

  fills params

- patterns:

  pattern params

- edges:

  edges params

- labels:

  labels params

- quantities:

  quantities params

- number:

  current diagram number

- merged_sets:

  sets that are the same and have been merged

## Value

A [`grid::gList()`](https://rdrr.io/r/grid/grid-defunct.html) is
returned.
