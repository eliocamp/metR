# Bilinear interpolation

Interpolates values using bilinear interpolation.

## Usage

``` r
Interpolate(formula, x.out, y.out, data = NULL, grid = TRUE, path = FALSE)
```

## Arguments

- formula:

  a formula indicating dependent and independent variables (see Details)

- x.out, y.out:

  x and y values where to interpolate (see Details)

- data:

  optional data.frame with the data

- grid:

  logical indicating if x.out and y.out define a regular grid.

- path:

  a logical or character indicating if the x.out and y.out define a
  path. If character, it will be the name of the column returning the
  order of said path.

## Value

A data.frame with interpolated values and locations

## Details

`formula` must be of the form VAR1 \| VAR2 ~ X + Y where VAR1, VAR2,
etc... are the names of the variables to interpolate and X and Y the
names of the x and y values, respectively. It is also possible to pass
only values of x, in which case, regular linear interpolation is
performed and y.out, if exists, is ignored with a warning.

If `grid = TRUE`, `x.out` and `y.out` must define the values of a
regular grid. If `grid = FALSE`, they define the locations where to
interpolate. Both `grid` and `path` cannot be set to `TRUE` and the
value of `path` takes precedence.

`x.out` can be a list, in which case, the first two elements will be
interpreted as the x and y values where to interpolate and it can also
have a `path` element that will be used in place of the `path` argument.
This helps when creating a path with
[as.path](https://eliocamp.github.io/metR/reference/as.path.md) (see
Examples)

## Examples

``` r
library(data.table)
data(geopotential)
geopotential <- geopotential[date == date[1]]
# new grid
x.out <- seq(0, 360, by = 10)
y.out <- seq(-90, 0, by = 10)

# Interpolate values to a new grid
interpolated <- geopotential[, Interpolate(gh ~ lon + lat, x.out, y.out)]

# Add values to an existing grid
geopotential[, gh.new := Interpolate(gh ~ lon + lat, lon, lat,
                                     data = interpolated, grid = FALSE)$gh]
#>         lon   lat   lev       gh       date   gh.new
#>       <num> <num> <int>    <num>     <Date>    <num>
#>    1:   0.0 -22.5   700 3163.839 1990-01-01       NA
#>    2:   2.5 -22.5   700 3162.516 1990-01-01       NA
#>    3:   5.0 -22.5   700 3162.226 1990-01-01       NA
#>    4:   7.5 -22.5   700 3162.323 1990-01-01       NA
#>    5:  10.0 -22.5   700 3163.097 1990-01-01       NA
#>   ---                                               
#> 4028: 347.5 -90.0   700 2715.936 1990-01-01 2715.936
#> 4029: 350.0 -90.0   700 2715.936 1990-01-01       NA
#> 4030: 352.5 -90.0   700 2715.936 1990-01-01       NA
#> 4031: 355.0 -90.0   700 2715.936 1990-01-01       NA
#> 4032: 357.5 -90.0   700 2715.936 1990-01-01       NA

# Interpolate multiple values
geopotential[, c("u", "v") := GeostrophicWind(gh, lon, lat)]
#>         lon   lat   lev       gh       date   gh.new     u           v
#>       <num> <num> <int>    <num>     <Date>    <num> <num>       <num>
#>    1:   0.0 -22.5   700 3163.839 1990-01-01       NA    NA  1.08181190
#>    2:   2.5 -22.5   700 3162.516 1990-01-01       NA    NA  0.55189199
#>    3:   5.0 -22.5   700 3162.226 1990-01-01       NA    NA  0.06625043
#>    4:   7.5 -22.5   700 3162.323 1990-01-01       NA    NA -0.29800162
#>    5:  10.0 -22.5   700 3163.097 1990-01-01       NA    NA -0.75064329
#>   ---                                                                 
#> 4028: 347.5 -90.0   700 2715.936 1990-01-01 2715.936    NA  0.00000000
#> 4029: 350.0 -90.0   700 2715.936 1990-01-01       NA    NA  0.00000000
#> 4030: 352.5 -90.0   700 2715.936 1990-01-01       NA    NA  0.00000000
#> 4031: 355.0 -90.0   700 2715.936 1990-01-01       NA    NA  0.00000000
#> 4032: 357.5 -90.0   700 2715.936 1990-01-01       NA    NA  0.00000000
interpolated <- geopotential[, Interpolate(u | v ~ lon + lat, x.out, y.out)]

# Interpolate values following a path
lats <- c(-34, -54, -30)   # start and end latitudes
lons <- c(302, 290, 180)   # start and end longituded
path <- geopotential[, Interpolate(gh ~ lon + lat, as.path(lons, lats))]
```
