# Promote a legacy 5-column ellipse data frame (h, k, a, b, phi) into the wide `$shapes` schema. Used by the `venn()` path, where the precomputed venn-shape lookup is still expressed as ellipses.

Promote a legacy 5-column ellipse data frame (h, k, a, b, phi) into the
wide `$shapes` schema. Used by the
[`venn()`](https://jolars.github.io/eulerr/reference/venn.md) path,
where the precomputed venn-shape lookup is still expressed as ellipses.

## Usage

``` r
ellipse_frame_to_shapes(fpar, shape)
```

## Arguments

- fpar:

  a 5-column (h, k, a, b, phi) ellipse data frame

- shape:

  the shape kind to tag every row with
