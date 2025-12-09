# Helpful scales for maps

These functions are simple wrappers around
[`scale_x_continuous`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
and
[`scale_y_continuous`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
with helpful defaults for plotting longitude, latitude and pressure
levels.

## Usage

``` r
scale_x_longitude(
  name = "",
  ticks = 30,
  breaks = seq(-180, 360, by = ticks),
  expand = c(0, 0),
  labels = LonLabel,
  ...
)

scale_y_longitude(
  name = "",
  ticks = 60,
  breaks = seq(-180, 360, by = ticks),
  expand = c(0, 0),
  labels = LonLabel,
  ...
)

scale_x_latitude(
  name = "",
  ticks = 30,
  breaks = seq(-90, 90, by = ticks),
  expand = c(0, 0),
  labels = LatLabel,
  ...
)

scale_y_latitude(
  name = "",
  ticks = 30,
  breaks = seq(-90, 90, by = ticks),
  expand = c(0, 0),
  labels = LatLabel,
  ...
)

scale_x_level(name = "", expand = c(0, 0), trans = "reverselog", ...)

scale_y_level(name = "", expand = c(0, 0), trans = "reverselog", ...)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- ticks:

  spacing between breaks

- breaks:

  One of:

  - `NULL` for no breaks

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks computed by the [transformation
    object](https://scales.r-lib.org/reference/new_transform.html)

  - A numeric vector of positions

  - A function that takes the limits as input and returns breaks as
    output (e.g., a function returned by
    [`scales::extended_breaks()`](https://scales.r-lib.org/reference/breaks_extended.html)).
    Note that for position scales, limits are provided after scale
    expansion. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- expand:

  For position scales, a vector of range expansion constants used to add
  some padding around the data to ensure that they are placed some
  distance away from the axes. Use the convenience function
  [`ggplot2::expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
  to generate the values for the expand argument.

- labels:

  One of the options below. Please note that when `labels` is a vector,
  it is highly recommended to also set the `breaks` argument as a vector
  to protect against unintended mismatches.

  - `NULL` for no labels

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default labels computed by the transformation object

  - A character vector giving labels (must be same length as `breaks`)

  - An expression vector (must be the same length as breaks). See
    ?plotmath for details.

  - A function that takes the breaks as input and returns labels as
    output. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

- ...:

  Other arguments passed on to `scale_(x|y)_continuous()`

- trans:

  **\[deprecated\]** Deprecated in favour of `transform`.

## See also

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
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
[`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md),
[`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

## Examples

``` r
data(geopotential)
library(ggplot2)
ggplot(geopotential[date == date[1]], aes(lon, lat, z = gh)) +
    geom_contour() +
    scale_x_longitude() +
    scale_y_latitude()


data(temperature)
ggplot(temperature[lon == lon[1] & lat == lat[1]], aes(air, lev)) +
    geom_path() +
    scale_y_level()

```
