# Print a summary of an Euler diagram

This function is responsible for printing fits from
[`euler()`](https://jolars.github.io/eulerr/reference/euler.md) and
provides a summary of the fit. Prints a data frame of the original set
relationships and the fitted values as well as `diagError` and `stress`
statistics.

## Usage

``` r
# S3 method for class 'euler'
print(x, round = 3, vsep = strrep("-", 0.75 * getOption("width")), ...)
```

## Arguments

- x:

  `'euler'` object from
  [`euler()`](https://jolars.github.io/eulerr/reference/euler.md)

- round:

  number of decimal places to round to

- vsep:

  character string to paste in between `euler` objects when `x` is a
  nested `euler` object

- ...:

  arguments passed to
  [`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Value

Summary statistics of the fitted Euler diagram are printed to screen.

## See also

[`euler()`](https://jolars.github.io/eulerr/reference/euler.md),
[`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Examples

``` r
euler(organisms)
#>                               original fitted residuals regionError
#> animal                               0  0.582    -0.582       0.086
#> mammal                               0  0.302    -0.302       0.044
#> plant                                0  0.210    -0.210       0.031
#> sea                                  0  0.430    -0.430       0.063
#> spiny                                0  0.166    -0.166       0.025
#> animal&mammal                        2  1.817     0.183       0.018
#> animal&plant                         0  0.000     0.000       0.000
#> animal&sea                           1  0.612     0.388       0.053
#> animal&spiny                         0  0.215    -0.215       0.032
#> mammal&plant                         0  0.000     0.000       0.000
#> mammal&sea                           1  0.000     1.000       0.143
#> mammal&spiny                         0  0.000     0.000       0.000
#> plant&sea                            1  0.868     0.132       0.015
#> plant&spiny                          1  0.000     1.000       0.143
#> sea&spiny                            0  0.176    -0.176       0.026
#> animal&mammal&plant                  0  0.000     0.000       0.000
#> animal&mammal&sea                    0  0.268    -0.268       0.040
#> animal&mammal&spiny                  0  0.061    -0.061       0.009
#> animal&plant&sea                     0  0.119    -0.119       0.018
#> animal&plant&spiny                   0  0.000     0.000       0.000
#> animal&sea&spiny                     1  0.715     0.285       0.037
#> mammal&plant&sea                     0  0.000     0.000       0.000
#> mammal&plant&spiny                   0  0.000     0.000       0.000
#> mammal&sea&spiny                     0  0.000     0.000       0.000
#> plant&sea&spiny                      0  0.016    -0.016       0.002
#> animal&mammal&plant&sea              0  0.000     0.000       0.000
#> animal&mammal&plant&spiny            0  0.000     0.000       0.000
#> animal&mammal&sea&spiny              0  0.177    -0.177       0.026
#> animal&plant&sea&spiny               0  0.043    -0.043       0.006
#> mammal&plant&sea&spiny               0  0.000     0.000       0.000
#> animal&mammal&plant&sea&spiny        0  0.000     0.000       0.000
#> 
#> diagError: 0.143 
#> stress:    0.352 
```
