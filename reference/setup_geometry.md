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

- n:

  number of vertices to use to render each ellipse

- merged_sets:

  which sets have been merged?

## Value

a list object with slots for the various objects
