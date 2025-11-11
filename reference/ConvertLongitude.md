# Converts between longitude conventions

Converts longitude from \[0, 360) to \[-180, 180) and vice versa.

## Usage

``` r
ConvertLongitude(lon, group = NULL, from = NULL)
```

## Arguments

- lon:

  numeric vector of longitude

- group:

  optional vector of groups (the same length as longitude) that will be
  split on the edges (see examples)

- from:

  optionally explicitly say from which convention to convert

## Value

If `group` is missing, a numeric vector the same length of lon. Else, a
list with vectors `lon` and `group`.

## Examples

``` r
library(ggplot2)
library(data.table)
data(geopotential)

ggplot(geopotential[date == date[1]], aes(lon, lat, z = gh)) +
    geom_contour(color = "black") +
    geom_contour(aes(x = ConvertLongitude(lon)))

if (requireNamespace("maps")) {
map <- setDT(map_data("world"))
map[, c("lon", "group2") := ConvertLongitude(long, group, from = 180)]

ggplot(map, aes(lon, lat, group = group2)) +
    geom_path()
}
#> Loading required namespace: maps

```
