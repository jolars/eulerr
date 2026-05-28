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
