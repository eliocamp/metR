# Filled 2d contours of a 3d surface

While ggplot2's
[`geom_contour`](https://ggplot2.tidyverse.org/reference/geom_contour.html)
can plot nice contours, it doesn't work with the polygon geom. This stat
makes some small manipulation of the data to ensure that all contours
are closed and also computes a new aesthetic `int.level`, which differs
from `level` (computed by
[ggplot2::geom_contour](https://ggplot2.tidyverse.org/reference/geom_contour.html))
in that represents the value of the `z` aesthetic *inside* the contour
instead of at the edge. It also computes breaks globally instead of per
panel, so that faceted plots have all the same binwidth.

## Usage

``` r
geom_contour_fill(
  mapping = NULL,
  data = NULL,
  stat = "ContourFill",
  position = "identity",
  ...,
  breaks = MakeBreaks(),
  bins = NULL,
  binwidth = NULL,
  proj = NULL,
  proj.latlon = TRUE,
  clip = NULL,
  kriging = FALSE,
  global.breaks = TRUE,
  na.fill = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_contour_fill(
  mapping = NULL,
  data = NULL,
  geom = "polygon",
  position = "identity",
  ...,
  breaks = MakeBreaks(),
  bins = NULL,
  binwidth = NULL,
  global.breaks = TRUE,
  proj = NULL,
  proj.latlon = TRUE,
  clip = NULL,
  kriging = FALSE,
  na.fill = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used the override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- breaks:

  numeric vector of breaks

- bins:

  Number of evenly spaced breaks.

- binwidth:

  Distance between breaks.

- proj:

  The projection to which to project the contours to. It can be either a
  projection string or a function to apply to the whole contour dataset.

- proj.latlon:

  Logical indicating if the projection step should project from a
  cartographic projection to a lon/lat grid or the other way around.

- clip:

  A simple features object to be used as a clip. Contours are only drawn
  in the interior of this polygon.

- kriging:

  Whether to perform ordinary kriging before contouring. Use this if you
  want to use contours with irregularly spaced data. If `FALSE`, no
  kriging is performed. If `TRUE`, kriging will be performed with 40
  points. If a numeric, kriging will be performed with `kriging` points.

- global.breaks:

  Logical indicating whether `breaks` should be computed for the whole
  data or for each grouping.

- na.fill:

  How to fill missing values.

  - `FALSE` for letting the computation fail with no interpolation

  - `TRUE` for imputing missing values with
    [Impute2D](https://eliocamp.github.io/metR/reference/Impute2D.md)

  - A numeric value for constant imputation

  - A function that takes a vector and returns a numeric (e.g. `mean`)

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

## Aesthetics

`geom_contour_fill` understands the following aesthetics (required
aesthetics are in bold):

- **x**

- **y**

- `alpha`

- `colour`

- `group`

- `linetype`

- `size`

- `weight`

## Computed variables

- level:

  An ordered factor that represents bin ranges.

- level_d:

  Same as `level`, but automatically uses
  [`scale_fill_discretised()`](https://eliocamp.github.io/metR/reference/discretised_scale.md)

- level_low,level_high,level_mid:

  Lower and upper bin boundaries for each band, as well the mid point
  between the boundaries.

## See also

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
[`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
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
surface <- reshape2::melt(volcano)
ggplot(surface, aes(Var1, Var2, z = value)) +
  geom_contour_fill() +
  geom_contour(color = "black", size = 0.1)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.


ggplot(surface, aes(Var1, Var2, z = value)) +
  geom_contour_fill(aes(fill = after_stat(level)))


ggplot(surface, aes(Var1, Var2, z = value)) +
  geom_contour_fill(aes(fill = after_stat(level_d)))
```
