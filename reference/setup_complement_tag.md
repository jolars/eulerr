# Setup the complement-count tag, in the same shape as a region tag so it shares one [`makeContent.EulerTags()`](https://jolars.github.io/eulerr/reference/makeContent.EulerTags.md) pass.

Acts as a quantity-only tag with `combo_key = ""`. The complement label
text comes either from `complement$label` (user override) or from
`container_data$quantity_label` (the fitted complement count).

## Usage

``` r
setup_complement_tag(container_data, complement, number)
```
