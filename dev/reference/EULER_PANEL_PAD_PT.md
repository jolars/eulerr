# Fixed padding in pt for the panel viewport scale.

Geometry / labels that land flush with the bbox would otherwise be
clipped at the device edge by stroke width and anti-aliasing — both of
which are in device units, not native units, so the padding is in pt
rather than a fraction of the coordinate range.

## Usage

``` r
EULER_PANEL_PAD_PT
```

## Format

An object of class `numeric` of length 1.
