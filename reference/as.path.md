# Interpolates between locations

This is a helper function to quickly make an interpolated list of
locations between a number of locations

## Usage

``` r
as.path(x, y, n = 10, path = TRUE)
```

## Arguments

- x, y:

  numeric vectors of x and y locations. If one of them is of length 1,
  if will be recycled.

- n:

  number of points to interpolate to

- path:

  either `TRUE` of a character vector with the name of the path.

## Value

A list of components `x` and `y` with the list of locations and the
`path` arguments

## Details

This function is mostly useful when combined with
[Interpolate](https://eliocamp.github.io/metR/reference/Interpolate.md)

## See also

Interpolate
