# Streamlines

Streamlines are paths that are always tangential to a vector field. In
the case of a steady field, it's identical to the path of a massless
particle that moves with the "flow".

## Usage

``` r
geom_streamline(
  mapping = NULL,
  data = NULL,
  stat = "streamline",
  position = "identity",
  ...,
  L = 5,
  min.L = 0,
  res = 1,
  S = NULL,
  dt = NULL,
  xwrap = NULL,
  ywrap = NULL,
  skip = 1,
  skip.x = skip,
  skip.y = skip,
  start = NULL,
  n = NULL,
  nx = n,
  ny = n,
  jitter = 1,
  jitter.x = jitter,
  jitter.y = jitter,
  arrow.angle = 6,
  arrow.length = 0.5,
  arrow.ends = "last",
  arrow.type = "closed",
  arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"), ends = arrow.ends,
    type = arrow.type),
  lineend = "butt",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_streamline(
  mapping = NULL,
  data = NULL,
  geom = "streamline",
  position = "identity",
  ...,
  L = 5,
  min.L = 0,
  res = 1,
  S = NULL,
  dt = NULL,
  xwrap = NULL,
  ywrap = NULL,
  skip = 1,
  skip.x = skip,
  skip.y = skip,
  start = NULL,
  n = NULL,
  nx = n,
  ny = n,
  jitter = 1,
  jitter.x = jitter,
  jitter.y = jitter,
  arrow.angle = 6,
  arrow.length = 0.5,
  arrow.ends = "last",
  arrow.type = "closed",
  arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"), ends = arrow.ends,
    type = arrow.type),
  lineend = "butt",
  na.rm = TRUE,
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

- L, :

  typical length of a streamline in x and y units

- min.L:

  minimum length of segments to show

- res, :

  resolution parameter (higher numbers increases the resolution)

- S:

  optional numeric number of timesteps for integration

- dt:

  optional numeric size "timestep" for integration

- xwrap, ywrap:

  vector of length two used to wrap the circular dimension.

- skip, skip.x, skip.y:

  numeric specifying number of gridpoints not to draw in the x and y
  direction

- start:

  optional list/data.frame with x and y columns giving the starting
  locations for integration.

- n, nx, ny:

  optional numeric indicating the number of points to draw in the x and
  y direction (replaces `skip` if not `NULL`)

- jitter, jitter.x, jitter.y:

  amount of jitter of the starting points

- arrow.length, arrow.angle, arrow.ends, arrow.type:

  parameters passed to [grid::arrow](https://rdrr.io/r/grid/arrow.html)

- arrow:

  specification for arrow heads, as created by
  [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html).

- lineend:

  Line end style (round, butt, square).

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

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

## Details

Streamlines are computed by simple integration with a forward Euler
method. By default, `stat_streamline()` computes `dt` and `S` from `L`,
`res`, the resolution of the grid and the mean magnitude of the field.
`S` is then defined as the number of steps necessary to make a
streamline of length `L` under an uniform mean field and `dt` is chosen
so that each step is no larger than the resolution of the data (divided
by the `res` parameter). Be aware that this rule of thumb might fail in
field with very skewed distribution of magnitudes.

Alternatively, `L` and/or `res` are ignored if `S` and/or `dt` are
specified explicitly. This not only makes it possible to fine-tune the
result but also divorces the integration parameters from the properties
of the data and makes it possible to compare streamlines between
different fields.

The starting grid is a semi regular grid defined, either by the
resolution of the field and the `skip.x` and `skip.y` parameters o the
`nx` and `ny` parameters, jittered by an amount proportional to the
resolution of the data and the `jitter.x` and `jitter.y` parameters.

It might be important that the units of the vector field are compatible
to the units of the x and y dimensions. For example, passing `dx` and
`dy` in m/s on a longitude-latitude grid will might misleading results
(see
[spherical](https://eliocamp.github.io/metR/reference/spherical.md)).

Missing values are not permitted and the field must be defined on a
regular grid, for now.

## Aesthetics

`stat_streamline` understands the following aesthetics (required
aesthetics are in bold)

- **x**

- **y**

- **dx**

- **dy**

- `alpha`

- `colour`

- `linetype`

- `size`

## Computed variables

- step:

  step in the simulation

- dx:

  dx at each location of the streamline

- dy:

  dy at each location of the streamline

## See also

Other ggplot2 helpers:
[`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md),
[`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md),
[`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md),
[`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
[`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md),
[`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md),
[`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md),
[`guide_colourstrip()`](https://eliocamp.github.io/metR/reference/guide_colourstrip.md),
[`map_labels`](https://eliocamp.github.io/metR/reference/map_labels.md),
[`reverselog_trans()`](https://eliocamp.github.io/metR/reference/reverselog_trans.md),
[`scale_divergent`](https://eliocamp.github.io/metR/reference/scale_divergent.md),
[`scale_longitude`](https://eliocamp.github.io/metR/reference/scale_longitude.md),
[`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md),
[`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(data.table)
library(ggplot2)
data(geopotential)

geopotential <- copy(geopotential)[date == date[1]]
geopotential[, gh.z := Anomaly(gh), by = .(lat)]
geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]

(g <- ggplot(geopotential, aes(lon, lat)) +
    geom_contour2(aes(z = gh.z), xwrap = c(0, 360)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 60,
                    xwrap = c(0, 360)))

# The circular parameter is particularly important for polar coordinates
g + coord_polar()

# If u and v are not converted into degrees/second, the resulting
# streamlines have problems, specially near the pole.
ggplot(geopotential, aes(lon, lat)) +
    geom_contour(aes(z = gh.z)) +
    geom_streamline(aes(dx = u, dy = v), L = 50)

# The step variable can be mapped to size or alpha to
# get cute "drops". It's important to note that after_stat(dx) (the calculated variable)
# is NOT the same as dx (from the data).
ggplot(geopotential, aes(lon, lat)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v), alpha = after_stat(step),
                        color = sqrt(after_stat(dx^2) + after_stat(dy^2)),
                        size = after_stat(step)),
                        L = 40, xwrap = c(0, 360), res = 2, arrow = NULL,
                        lineend = "round") +
    scale_size(range = c(0, 0.6))

# Using topographic information to simulate "rivers" from slope
topo <- GetTopography(295, -55+360, -30, -42, res = 1/20)  # needs internet!
topo[, c("dx", "dy") := Derivate(h ~ lon + lat)]
topo[h <= 0, c("dx", "dy") := 0]

# See how in this example the integration step is too coarse in the
# western montanous region where the slope is much higher than in the
# flatlands of La Pampa at in the east.
ggplot(topo, aes(lon, lat)) +
    geom_relief(aes(z = h), interpolate = TRUE, data = topo[h >= 0]) +
    geom_contour(aes(z = h), breaks = 0, color = "black") +
    geom_streamline(aes(dx = -dx, dy = -dy), L = 10, skip = 3, arrow = NULL,
                    color = "#4658BD") +
    coord_quickmap()
 } # }
```
