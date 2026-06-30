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
  placement_opts = NULL,
  combo_labels = NULL
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

- combo_labels:

  region labels in the order of the per-region graphical parameters
  (`fills$gp`, `patterns$gp`), used to associate each region's geometry
  with its parameters by name rather than position

## Value

A [`grid::gList()`](https://rdrr.io/r/grid/grid.grob.html) is returned.
