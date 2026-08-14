# Measure every drawable tag inside `tags_grob` against the current viewport. Returns parallel vectors of combo / width / height suitable for handing to [`place_euler_labels()`](https://jolars.github.io/eulerr/reference/place_euler_labels.md), plus the resolved leader gap in native units (so the FFI sees one number per draw pass).

Measure every drawable tag inside `tags_grob` against the current
viewport. Returns parallel vectors of combo / width / height suitable
for handing to
[`place_euler_labels()`](https://jolars.github.io/eulerr/reference/place_euler_labels.md),
plus the resolved leader gap in native units (so the FFI sees one number
per draw pass).

## Usage

``` r
measure_all_tags(tags_grob, padding, gap = NULL)
```

## Arguments

- tags_grob:

  an `EulerTags` gTree

- padding:

  vertical separation between tag components, as a
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html)

- gap:

  leader-tip gap; see
  [`resolve_gap_native()`](https://jolars.github.io/eulerr/reference/resolve_gap_native.md)
