# Open a temporary grid measurement device + viewport.

Returns an idempotent closer thunk that pops the viewport and (when we
opened one) closes the null PDF device. Used so we can call
`grid::convertWidth(grobWidth(...), "native", ...)` at setup time — i.e.
before
[`plot.eulergram()`](https://jolars.github.io/eulerr/dev/reference/plot.eulergram.md)
ever opens a real device — to size the label boxes that drive label
placement.

## Usage

``` r
open_measurement_viewport(xlim, ylim)
```
