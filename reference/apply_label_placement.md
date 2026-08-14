# Run eunoia label placement, expanding limits so exterior labels are not clipped. Drives one initial pass plus one re-measure pass when the limits widened by more than `re_measure_threshold` on the short side. Updates `centers` (and the complement slot on `container_data`) in place with placed `(x, y)` plus `kind`, `tether_x`, `tether_y`, `leader_end_x`, `leader_end_y`.

When `placement_opts` is `NULL`, defaults to eunoia's raycast + POI
tether.

## Usage

``` r
apply_label_placement(
  centers,
  container_data,
  shapes,
  labels,
  quantities,
  annotations = NULL,
  placement_opts = NULL,
  do_complement_label = FALSE,
  limits,
  n_vertices,
  label_precision,
  re_measure_threshold = 0.01
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

- labels, quantities, annotations:

  the three tag component parameter lists, each with a `$gp`

- placement_opts:

  placement options, or `NULL` for eunoia's defaults

- do_complement_label:

  whether the complement gets a tag too

- limits:

  a list of `xlim`/`ylim` to place against and widen

- n_vertices:

  number of vertices used to discretize each shape

- label_precision:

  number of decimals used when rendering quantities

- re_measure_threshold:

  relative widening of the short side that triggers a second placement
  pass

## Details

Returns a list with `centers`, `container_data`, and `limits`.
