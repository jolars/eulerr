# Measure every drawable tag inside `tags_grob` against the current viewport. Returns parallel vectors of combo / width / height suitable for handing to [`place_euler_labels()`](https://jolars.github.io/eulerr/dev/reference/place_euler_labels.md), plus the resolved leader gap in native units (so the FFI sees one number per draw pass).

Measure every drawable tag inside `tags_grob` against the current
viewport. Returns parallel vectors of combo / width / height suitable
for handing to
[`place_euler_labels()`](https://jolars.github.io/eulerr/dev/reference/place_euler_labels.md),
plus the resolved leader gap in native units (so the FFI sees one number
per draw pass).

## Usage

``` r
measure_all_tags(tags_grob, padding, gap = NULL)
```
