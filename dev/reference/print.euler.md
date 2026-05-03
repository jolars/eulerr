# Print a summary of an Euler diagram

This function is responsible for printing fits from
[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md) and
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
  [`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md)

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

[`euler()`](https://jolars.github.io/eulerr/dev/reference/euler.md),
[`base::print.data.frame()`](https://rdrr.io/r/base/print.dataframe.html)

## Examples

``` r
euler(organisms)
#>                   original fitted residuals regionError
#> animal                   0  0.463    -0.463       0.075
#> mammal                   0  0.215    -0.215       0.035
#> plant                    0  0.371    -0.371       0.060
#> sea                      0  0.330    -0.330       0.053
#> spiny                    0  0.175    -0.175       0.028
#> animal&mammal            2  1.904     0.096       0.022
#> animal&sea               1  0.570     0.430       0.051
#> mammal&sea               1  0.000     1.000       0.143
#> plant&sea                1  0.804     0.196       0.013
#> plant&spiny              1  0.875     0.125       0.002
#> animal&mammal&sea        0  0.250    -0.250       0.040
#> animal&plant&sea         0  0.034    -0.034       0.006
#> animal&sea&spiny         1  0.000     1.000       0.143
#> plant&sea&spiny          0  0.203    -0.203       0.033
#> 
#> diagError: 0.143 
#> stress:    0.321 
```
