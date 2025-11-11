# Percentiles

Computes percentiles.

## Usage

``` r
Percentile(x)
```

## Arguments

- x:

  numeric vector

## Value

A numeric vector of the same length as x with the percentile of each
value of x.

## See also

Other utilities:
[`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md),
[`JumpBy()`](https://eliocamp.github.io/metR/reference/JumpBy.md),
[`Mag()`](https://eliocamp.github.io/metR/reference/Mag.md),
[`logic`](https://eliocamp.github.io/metR/reference/logic.md)

## Examples

``` r
x <- rnorm(100)
p <- Percentile(x)
```
