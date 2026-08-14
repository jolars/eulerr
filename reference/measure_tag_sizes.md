# Measure all candidate tag sizes (regions + optional complement) inside a fresh measurement viewport scaled to `xlim`/`ylim`.

Measure all candidate tag sizes (regions + optional complement) inside a
fresh measurement viewport scaled to `xlim`/`ylim`.

## Usage

``` r
measure_tag_sizes(
  centers,
  do_complement_label,
  complement_label,
  labels_gp,
  quantities_gp,
  annotations_gp,
  padding,
  gap,
  xlim,
  ylim
)
```

## Arguments

- centers:

  the per-region tag data frame from
  [`setup_geometry()`](https://jolars.github.io/eulerr/reference/setup_geometry.md)

- do_complement_label:

  whether to measure a complement tag as well

- complement_label:

  the complement tag's parameters

- labels_gp, quantities_gp, annotations_gp:

  per-component [`grid::gpar()`](https://rdrr.io/r/grid/gpar.html) lists

- padding:

  vertical separation between tag components, as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html)

- gap:

  leader-tip gap; see
  [`resolve_gap_native()`](https://jolars.github.io/eulerr/reference/resolve_gap_native.md)

- xlim, ylim:

  native scales of the measurement viewport
