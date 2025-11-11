# Wrap periodic data to any range

Periodic data can be defined only in one period and be extended to any
arbitrary range.

## Usage

``` r
WrapCircular(x, circular = "lon", wrap = c(0, 360))
```

## Arguments

- x:

  a data.frame

- circular:

  the name of the circular dimension

- wrap:

  the wrap for the data to be extended to

## Value

A data.frame.

## See also

geom_contour2

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
[`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
[`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md),
[`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md),
[`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md),
[`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md),
[`guide_colourstrip()`](https://eliocamp.github.io/metR/reference/guide_colourstrip.md),
[`map_labels`](https://eliocamp.github.io/metR/reference/map_labels.md),
[`reverselog_trans()`](https://eliocamp.github.io/metR/reference/reverselog_trans.md),
[`scale_divergent`](https://eliocamp.github.io/metR/reference/scale_divergent.md),
[`scale_longitude`](https://eliocamp.github.io/metR/reference/scale_longitude.md),
[`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md),
[`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

## Examples

``` r
library(ggplot2)
library(data.table)
data(geopotential)
g <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    geom_contour(aes(z = gh)) +
    coord_polar() +
    ylim(c(-90, -10))

# This plot has problems in lon = 0
g


# But using WrapCircular solves it.
g %+% WrapCircular(geopotential[date == date[1]], "lon", c(0, 360))
#> Warning: <ggplot> %+% x was deprecated in ggplot2 4.0.0.
#> ℹ Please use <ggplot> + x instead.
#> Warning: 'WrapCircular' is deprecated, use ggperiodic::wrap instead.


# Aditionally data can be just repeatet to the right and
# left
ggplot(WrapCircular(geopotential[date == date[1]], wrap = c(-180, 360 + 180)),
       aes(lon, lat)) +
    geom_contour(aes(z = gh))
#> Warning: 'WrapCircular' is deprecated, use ggperiodic::wrap instead.


# The same behaviour is now implemented directly in geom_contour2
# and geom_contour_fill
ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    geom_contour2(aes(z = gh), xwrap = c(-180, 360 + 180))
#> Warning: 'xwrap' and 'ywrap' will be deprecated. Use ggperiodic::periodic insead.

```
