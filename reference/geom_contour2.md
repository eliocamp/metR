# 2d contours of a 3d surface

Similar to
[ggplot2::geom_contour](https://ggplot2.tidyverse.org/reference/geom_contour.html)
but it can label contour lines, accepts accepts a function as the
`breaks` argument and and computes breaks globally instead of per panel.

## Usage

``` r
geom_contour2(
  mapping = NULL,
  data = NULL,
  stat = "contour2",
  position = "identity",
  ...,
  lineend = "butt",
  linejoin = "round",
  linemitre = 1,
  breaks = MakeBreaks(),
  bins = NULL,
  binwidth = NULL,
  global.breaks = TRUE,
  na.rm = FALSE,
  na.fill = FALSE,
  skip = 1,
  margin = grid::unit(c(1, 1, 1, 1), "pt"),
  label.placer = label_placer_flattest(),
  show.legend = NA,
  inherit.aes = TRUE
)

stat_contour2(
  mapping = NULL,
  data = NULL,
  geom = "contour2",
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
  na.rm = FALSE,
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
  can be used to override the default coupling between geoms and stats.
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

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

- breaks:

  One of:

  - A numeric vector of breaks

  - A function that takes the range of the data and binwidth as input
    and returns breaks as output

- bins:

  Number of evenly spaced breaks.

- binwidth:

  Distance between breaks.

- global.breaks:

  Logical indicating whether `breaks` should be computed for the whole
  data or for each grouping.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- na.fill:

  How to fill missing values.

  - `FALSE` for letting the computation fail with no interpolation

  - `TRUE` for imputing missing values with
    [Impute2D](https://eliocamp.github.io/metR/reference/Impute2D.md)

  - A numeric value for constant imputation

  - A function that takes a vector and returns a numeric (e.g. `mean`)

- skip:

  number of contours to skip for labelling (e.g. `skip = 1` will skip 1
  contour line between labels).

- margin:

  the margin around labels around which contour lines are clipped to
  avoid overlapping.

- label.placer:

  a label placer function. See
  [`label_placer_flattest()`](https://eliocamp.github.io/metR/reference/label_placers.md).

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

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

## Aesthetics

`geom_contour2` understands the following aesthetics (required
aesthetics are in bold):

Aesthetics related to contour lines:

- **x**

- **y**

- **z**

- `alpha`

- `colour`

- `group`

- `linetype`

- `size`

- `weight`

Aesthetics related to labels:

- `label`

- `label_colour`

- `label_alpha`

- `label_size`

- `family`

- `fontface`

## Computed variables

- level:

  height of contour

## See also

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
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

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
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

# Breaks can be a function.
ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
    geom_contour2(aes(z = value, color = after_stat(level)),
                  breaks = AnchorBreaks(130, binwidth = 10))


# Add labels by supplying the label aes.
ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
    geom_contour2(aes(z = value, label = after_stat(level)))


ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
    geom_contour2(aes(z = value, label = after_stat(level)),
                  skip = 0)


# Use label.placer to control where contours are labelled.
ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
    geom_contour2(aes(z = value, label = after_stat(level)),
                      label.placer = label_placer_n(n = 2))


# Use the rot_adjuster argument of the placer function to
# control the angle. For example, to fix it to some angle:
ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
    geom_contour2(aes(z = value, label = after_stat(level)),
                  skip = 0,
                  label.placer = label_placer_flattest(rot_adjuster = 0))
```
