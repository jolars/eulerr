# Clip a (possibly multi-polygon) subject path against a single clip polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr actually uses at the stripe-pattern site.

Clip a (possibly multi-polygon) subject path against a single clip
polygon. Mirrors the slice of `polyclip::polyclip` behavior eulerr
actually uses at the stripe-pattern site.

## Usage

``` r
polygon_clip_rust(subject_x, subject_y, subject_id_lengths, clip_x, clip_y, op)
```
