# Per-shape bounding box dispatch. Reads the `type` tag on `shapes` (rows are assumed to share a tag since a diagram fixes one shape kind), then picks the appropriate width/height calculation. Falls back to the rotated-ellipse formula when the type is unknown so external callers constructing ad-hoc `$shapes` frames still get a sensible box.

Per-shape bounding box dispatch. Reads the `type` tag on `shapes` (rows
are assumed to share a tag since a diagram fixes one shape kind), then
picks the appropriate width/height calculation. Falls back to the
rotated-ellipse formula when the type is unknown so external callers
constructing ad-hoc `$shapes` frames still get a sensible box.

## Usage

``` r
shape_bounding_box(shapes)
```
