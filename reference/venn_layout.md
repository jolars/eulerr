# Canonical (non-proportional) Venn layout for a given shape.

Returns per-set geometry for a true Venn diagram of `set_names.len()`
sets, where every one of the `2^n - 1` regions is present. Used by the R
[`venn()`](https://jolars.github.io/eulerr/reference/venn.md) path for
shapes whose layout is supplied by eunoia rather than by eulerr's
precomputed ellipse table. Currently only `"rotated_rectangle"` is wired
here (it supports `n` in 1..=4); the rotation is what lets four
rectangles open all 15 regions, which axis-aligned rectangles cannot.

## Usage

``` r
venn_layout(set_names, shape)
```
