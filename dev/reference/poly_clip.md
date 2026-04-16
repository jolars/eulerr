# Polygon Clipping

This function is provided to efficiently and safely handle clipping
operations. It wraps around
[`polyclip::polyclip()`](https://rdrr.io/pkg/polyclip/man/polyclip.html),
which is an interface to the **Clipper** C++ library.

## Usage

``` r
poly_clip(a, b, op = c("intersection", "union", "minus", "xor"))
```

## Arguments

- a:

  polygon

- b:

  polygon

- op:

  operation

## Value

A list of lists.
