# Single placement pass: measure tags, call the Rust FFI, return the placement records and the canvas bbox returned by eunoia.

Single placement pass: measure tags, call the Rust FFI, return the
placement records and the canvas bbox returned by eunoia.

## Usage

``` r
run_placement_pass(
  centers,
  container_data,
  shapes,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding,
  placement_opts,
  do_complement_label,
  xlim,
  ylim,
  n_vertices,
  label_precision
)
```

## Arguments

- centers:

  the per-region tag data frame from
  [`setup_geometry()`](https://jolars.github.io/eulerr/reference/setup_geometry.md)

- container_data:

  the complement (container) region's data

- shapes:

  the diagram's `$shapes` data frame

- labels_gp, quantities_gp, annotations_gp:

  per-component [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) lists

- padding:

  vertical separation between tag components, as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html)

- placement_opts:

  resolved placement options; see
  [`resolve_placement_opts()`](https://jolars.github.io/eulerr/reference/resolve_placement_opts.md)

- do_complement_label:

  whether the complement gets a tag too

- xlim, ylim:

  native scales of the measurement viewport

- n_vertices:

  number of vertices used to discretize each shape

- label_precision:

  number of decimals used when rendering quantities
