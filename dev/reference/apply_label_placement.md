# Run eunoia label placement, expanding limits so exterior labels are not clipped. Drives one initial pass plus one re-measure pass when the limits widened by more than `re_measure_threshold` on the short side. Updates `centers` (and the complement slot on `container_data`) in place with placed `(x, y)` plus `kind`, `tether_x`, `tether_y`, `leader_end_x`, `leader_end_y`.

When `placement_opts` is `NULL`, defaults to eunoia's raycast + POI
tether.

## Usage

``` r
apply_label_placement(
  centers,
  container_data,
  ellipses,
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

## Details

Returns a list with `centers`, `container_data`, and `limits`.
