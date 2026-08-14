# Setup grobs for one tag (label + quantity + annotation + leader).

Builds the gList via
[`build_tag_grobs()`](https://jolars.github.io/eulerr/reference/build_tag_grobs.md)
and stashes the text / gpar bundle on the resulting `EulerTag` gTree so
[`makeContent.EulerTags()`](https://jolars.github.io/eulerr/reference/makeContent.EulerTags.md)
can rebuild it at draw time with fresh measurements.

## Usage

``` r
setup_tag(data, labels, quantities, annotations, number)
```

## Arguments

- data:

  one row of the placed `centers` data frame

- labels, quantities, annotations:

  the three tag component parameter lists, each with a `$gp`

- number:

  the diagram's region index, used to name the grobs
