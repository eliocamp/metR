# Skip observations

Skip observations

## Usage

``` r
JumpBy(x, by, start = 1, fill = NULL)
```

## Arguments

- x:

  vector

- by:

  numeric interval between elements to keep

- start:

  index to start from

- fill:

  how observations are skipped

## Value

A vector of the same class as x and, if `fill` is not `null`, the same
length.

## Details

Mostly useful for labelling only every `by`th element.

## See also

Other utilities:
[`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md),
[`Mag()`](https://eliocamp.github.io/metR/reference/Mag.md),
[`Percentile()`](https://eliocamp.github.io/metR/reference/Percentile.md),
[`logic`](https://eliocamp.github.io/metR/reference/logic.md)

## Examples

``` r
x <- 1:50
JumpBy(x, 2)   # only odd numbers
#>  [1]  1  3  5  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49
JumpBy(x, 2, start = 2)   # only even numbers
#>  [1]  2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50
JumpBy(x, 2, fill = NA)   # even numbers replaced by NA
#>  [1]  1 NA  3 NA  5 NA  7 NA  9 NA 11 NA 13 NA 15 NA 17 NA 19 NA 21 NA 23 NA 25
#> [26] NA 27 NA 29 NA 31 NA 33 NA 35 NA 37 NA 39 NA 41 NA 43 NA 45 NA 47 NA 49 NA
JumpBy(x, 2, fill = 6)   # even numbers replaced by 6
#>  [1]  1  6  3  6  5  6  7  6  9  6 11  6 13  6 15  6 17  6 19  6 21  6 23  6 25
#> [26]  6 27  6 29  6 31  6 33  6 35  6 37  6 39  6 41  6 43  6 45  6 47  6 49  6
```
