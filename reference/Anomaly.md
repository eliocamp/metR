# Anomalies

Saves keystrokes for computing anomalies.

## Usage

``` r
Anomaly(x, baseline = seq_along(x), ...)
```

## Arguments

- x:

  numeric vector

- baseline:

  logical or numerical vector used for subsetting x before computing the
  mean

- ...:

  other arguments passed to [`mean`](https://rdrr.io/r/base/mean.html)
  such as `na.rm`

## Value

A numeric vector of the same length as x with each value's distance to
the mean.

## See also

Other utilities:
[`JumpBy()`](https://eliocamp.github.io/metR/reference/JumpBy.md),
[`Mag()`](https://eliocamp.github.io/metR/reference/Mag.md),
[`Percentile()`](https://eliocamp.github.io/metR/reference/Percentile.md),
[`logic`](https://eliocamp.github.io/metR/reference/logic.md)

## Examples

``` r
# Zonal temperature anomaly
library(data.table)
temperature[, .(lon = lon, air.z = Anomaly(air)), by = .(lat, lev)]
#>           lat   lev   lon air.z
#>         <num> <int> <num> <num>
#>      1:    90  1000   0.0     0
#>      2:    90  1000   2.5     0
#>      3:    90  1000   5.0     0
#>      4:    90  1000   7.5     0
#>      5:    90  1000  10.0     0
#>     ---                        
#> 178700:   -90    10 347.5     0
#> 178701:   -90    10 350.0     0
#> 178702:   -90    10 352.5     0
#> 178703:   -90    10 355.0     0
#> 178704:   -90    10 357.5     0
```
