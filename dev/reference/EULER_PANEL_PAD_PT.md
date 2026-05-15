# Set the panel viewport's `xscale`/`yscale` at draw time.

Fires before grid pushes the panel viewport. We can therefore measure
labels against the live cell, run eunoia's placement, and compute a
viewport bbox that fits both the diagram and the labels. On window
resize grid invalidates the gTree and `makeContext` re-runs, so the
panel grows or shrinks to track the current device and exterior labels
never extend past the viewport edge.

## Usage

``` r
EULER_PANEL_PAD_PT
```

## Format

An object of class `numeric` of length 1.

## Details

Aspect preservation: the new bbox keeps `xrng / yrng` equal to the
geometry's natural aspect (set by
[`setup_geometry()`](https://jolars.github.io/eulerr/dev/reference/setup_geometry.md))
so that circles render as circles. The smaller dimension is padded if
the label-driven canvas bbox is asymmetric.
