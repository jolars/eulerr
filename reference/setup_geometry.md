# Compute geometries and label locations

Compute geometries and label locations

## Usage

``` r
setup_geometry(
  x,
  fills,
  edges,
  labels,
  quantities,
  annotations,
  n,
  merged_sets,
  placement_opts = NULL,
  do_complement_label = FALSE
)
```

## Arguments

- x:

  an object of class 'euler'

- fills:

  fills

- edges:

  edges

- labels:

  labels

- quantities:

  quantities

- annotations:

  annotations

- n:

  number of vertices to use to render each ellipse

- merged_sets:

  which sets have been merged?

- placement_opts:

  label placement options, or `NULL` for the defaults

- do_complement_label:

  whether to place a label for the complement region

## Value

a list object with slots for the various objects
