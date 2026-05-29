# Re-place tags at draw time so resizing the device doesn't clip labels.

The panel viewport is active when `makeContent` fires, so we can measure
each tag's footprint in current native units via
`grid::convertWidth(grobWidth(...), "native")` and call eunoia's
[`place_euler_labels()`](https://jolars.github.io/eulerr/reference/place_euler_labels.md)
again. On resize, grid invalidates the tree and `makeContent` re-runs,
so the placement tracks the current device automatically.

## Usage

``` r
# S3 method for class 'EulerTags'
makeContent(x)
```

## Details

Tags whose measured size is zero (or whose anchor / kind eunoia can't
compute) keep the positions they were built with — typically the
setup-time placement stored on `centers$x` / `centers$y`.
