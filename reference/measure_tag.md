# Native-unit AABB of one composite tag (label stacked above quantity, annotation stacked below quantity, separated by `padding`). The geometry matches what `setup_tag()` renders at draw time, so the size handed to eunoia agrees with the actual on-screen footprint.

Native-unit AABB of one composite tag (label stacked above quantity,
annotation stacked below quantity, separated by `padding`). The geometry
matches what
[`setup_tag()`](https://jolars.github.io/eulerr/reference/setup_tag.md)
renders at draw time, so the size handed to eunoia agrees with the
actual on-screen footprint.

## Usage

``` r
measure_tag(
  label,
  quantity,
  annotation,
  labels_par_id,
  quantities_par_id,
  annotations_par_id,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding_native
)
```

## Arguments

- label, quantity, annotation:

  the three text components of the tag; any of them may be `NULL`

- labels_par_id, quantities_par_id, annotations_par_id:

  per-component graphical parameter indices; `NA` or `NULL` means the
  component isn't drawn

- labels_gp, quantities_gp, annotations_gp:

  per-component [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) lists

- padding_native:

  vertical separation between components, in native units
