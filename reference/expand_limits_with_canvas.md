# Union the current `xlim`/`ylim` with the canvas bbox reported by eunoia. Returns the (possibly widened) limits.

`slack` pads the canvas bbox before the union (multiplicative, centered
on the bbox center). The setup-time bbox is sized for one reference
device; padding gives the draw-time placement headroom when the user
resizes the device smaller than that reference and labels grow in native
units accordingly. A `slack` of 1.4 absorbs roughly a 30 % linear resize
before exterior labels start to fall outside the panel viewport.

## Usage

``` r
expand_limits_with_canvas(limits, placements, slack = 1.4)
```
