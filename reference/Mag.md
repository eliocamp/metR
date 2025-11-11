# Magnitude and angle of a vector

Computes the magnitude of a vector of any dimension. Or angle (in
degrees) in 2 dimensions.

## Usage

``` r
Mag(...)

Angle(x, y)
```

## Arguments

- ...:

  numeric vectors of coordinates or list of coordinates

- x, y, :

  x and y directions of the vector

## Value

`Mag`: A numeric vector the same length as each element of ... that is
\\\sqrt(x^2 + y^2 + ...)\\. `Angle`: A numeric vector of the same length
as x and y that is `atan2(y, x)*180/pi`.

## Details

Helpful to save keystrokes and gain readability when computing wind (or
any other vector quantity) magnitude.

## See also

Other utilities:
[`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md),
[`JumpBy()`](https://eliocamp.github.io/metR/reference/JumpBy.md),
[`Percentile()`](https://eliocamp.github.io/metR/reference/Percentile.md),
[`logic`](https://eliocamp.github.io/metR/reference/logic.md)

Other utilities:
[`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md),
[`JumpBy()`](https://eliocamp.github.io/metR/reference/JumpBy.md),
[`Percentile()`](https://eliocamp.github.io/metR/reference/Percentile.md),
[`logic`](https://eliocamp.github.io/metR/reference/logic.md)

## Examples

``` r
Mag(10, 10)
#> [1] 14.14214
Angle(10, 10)
#> [1] 45
Mag(10, 10, 10, 10)
#> [1] 20
Mag(list(10, 10, 10, 10))
#> [1] 20

# There's no vector recicling!
if (FALSE) { # \dontrun{
Mag(1, 1:2)
} # }
```
