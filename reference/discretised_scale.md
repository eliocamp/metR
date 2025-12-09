# Create discretised versions of continuous scales

This scale allows ggplot to understand data that has been discretised
with some procedure akin to `cut` and access the underlying continuous
values. For a scale that does the opposite (take continuous data and
treat them as discrete) see
[`ggplot2::binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.html).

## Usage

``` r
as.discretised_scale(scale_function)

scale_fill_discretised(
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = ggplot2::guide_colorsteps(even.steps = FALSE, show.limits = TRUE),
  aesthetics = "fill"
)

scale_fill_divergent_discretised(
  ...,
  low = scales::muted("blue"),
  mid = "white",
  high = scales::muted("red"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = ggplot2::guide_colorsteps(even.steps = FALSE, show.limits = TRUE)
)

discretised_scale(
  aesthetics,
  scale_name,
  palette,
  name = ggplot2::waiver(),
  breaks = ggplot2::waiver(),
  labels = ggplot2::waiver(),
  limits = NULL,
  trans = scales::identity_trans(),
  na.value = NA,
  drop = FALSE,
  guide = ggplot2::guide_colorsteps(even.steps = FALSE),
  position = "left",
  rescaler = scales::rescale,
  oob = scales::censor,
  super = ScaleDiscretised
)
```

## Arguments

- scale_function:

  a scale function (e.g. `scale_fill_divergent`)

- ...:

  Arguments passed on to
  [`continuous_scale`](https://ggplot2.tidyverse.org/reference/continuous_scale.html)

  `scale_name`

  :   **\[deprecated\]** The name of the scale that should be used for
      error messages associated with this scale.

  `breaks`

  :   One of:

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

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `n.breaks`

  :   An integer guiding the number of major breaks. The algorithm may
      choose a slightly different number to ensure nice break labels.
      Will only have an effect if `breaks = waiver()`. Use `NULL` to use
      the default number of breaks given by the transformation.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale range

      - A numeric vector of length two providing limits of the scale.
        Use `NA` to refer to the existing minimum or maximum

      - A function that accepts the existing (automatic) limits and
        returns new limits. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. Note that setting limits on positional scales
        will **remove** data outside of the limits. If the purpose is to
        zoom, use the limit argument in the coordinate system (see
        [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

  `rescaler`

  :   A function used to scale the input values to the range \[0, 1\].
      This is always
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html),
      except for diverging and n colour gradients (i.e.,
      [`scale_colour_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html),
      [`scale_colour_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)).
      The `rescaler` is ignored by position scales, which always use
      [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html).
      Also accepts rlang
      [lambda](https://rlang.r-lib.org/reference/as_function.html)
      function notation.

  `oob`

  :   One of:

      - Function that handles limits outside of the scale limits (out of
        bounds). Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

      - The default
        ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
        replaces out of bounds values with `NA`.

      - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
        for squishing out of bounds values into range.

      - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
        for squishing infinite values into range.

  `trans`

  :   **\[deprecated\]** Deprecated in favour of `transform`.

  `call`

  :   The `call` used to construct the scale for reporting messages.

  `super`

  :   The super class to use for the constructed scale

- low, high:

  Colours for low and high ends of the gradient.

- space:

  colour space in which to calculate gradient. Must be "Lab" - other
  values are deprecated.

- na.value:

  Colour to use for missing values

- guide:

  Type of legend. Use `"colourbar"` for continuous colour bar, or
  `"legend"` for discrete colour legend.

- aesthetics:

  Character string or vector of character strings listing the name(s) of
  the aesthetic(s) that this scale works with. This can be useful, for
  example, to apply colour settings to the `colour` and `fill`
  aesthetics at the same time, via `aesthetics = c("colour", "fill")`.

- mid:

  colour for mid point

- midpoint:

  The midpoint (in data value) of the diverging scale. Defaults to 0.

- scale_name:

  **\[deprecated\]** The name of the scale that should be used for error
  messages associated with this scale.

- palette:

  A palette function that when called with a numeric vector with values
  between 0 and 1 returns the corresponding output values (e.g.,
  [`scales::pal_area()`](https://scales.r-lib.org/reference/pal_area.html)).

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

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

- limits:

  One of:

  - `NULL` to use the default scale range

  - A numeric vector of length two providing limits of the scale. Use
    `NA` to refer to the existing minimum or maximum

  - A function that accepts the existing (automatic) limits and returns
    new limits. Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation. Note that setting limits on positional scales
    will **remove** data outside of the limits. If the purpose is to
    zoom, use the limit argument in the coordinate system (see
    [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)).

- trans:

  **\[deprecated\]** Deprecated in favour of `transform`.

- drop:

  Should unused factor levels be omitted from the scale? The default,
  TRUE, uses the levels that appear in the data; FALSE uses all the
  levels in the factor.

- position:

  For position scales, The position of the axis. `left` or `right` for y
  axes, `top` or `bottom` for x axes.

- rescaler:

  A function used to scale the input values to the range \[0, 1\]. This
  is always
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html),
  except for diverging and n colour gradients (i.e.,
  [`scale_colour_gradient2()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html),
  [`scale_colour_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)).
  The `rescaler` is ignored by position scales, which always use
  [`scales::rescale()`](https://scales.r-lib.org/reference/rescale.html).
  Also accepts rlang
  [lambda](https://rlang.r-lib.org/reference/as_function.html) function
  notation.

- oob:

  One of:

  - Function that handles limits outside of the scale limits (out of
    bounds). Also accepts rlang
    [lambda](https://rlang.r-lib.org/reference/as_function.html)
    function notation.

  - The default
    ([`scales::censor()`](https://scales.r-lib.org/reference/oob.html))
    replaces out of bounds values with `NA`.

  - [`scales::squish()`](https://scales.r-lib.org/reference/oob.html)
    for squishing out of bounds values into range.

  - [`scales::squish_infinite()`](https://scales.r-lib.org/reference/oob.html)
    for squishing infinite values into range.

- super:

  The super class to use for the constructed scale

## Value

A function with the same arguments as `scale_function` that works with
discretised values.

## Details

This scale makes it very easy to synchronise the breaks of filled
contours and the breaks shown no the colour guide. Bear in mind that
when using
[`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md),
the default fill aesthetic (`level_mid`) is **not** discretised. To use
this scale with that geom, you need to set
`aes(fill = after_stat(level))`.

## See also

scale_fill_discretised

## Examples

``` r
library(ggplot2)
scale_fill_brewer_discretised <- as.discretised_scale(scale_fill_distiller)


library(ggplot2)

# Using the `level` compute aesthetic from `geom_contour_fill()`
# (or ggplot2::geom_contour_filled()), the default scale is discrete.
# This means that you cannot map colours to the underlying numbers.
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour_fill(aes(fill = after_stat(level)))


v + geom_contour_fill(aes(fill = after_stat(level))) +
  scale_fill_discretised()


# The scale can be customised the same as any continuous colour scale
v + geom_contour_fill(aes(fill = after_stat(level))) +
  scale_fill_discretised(low = "#a62100", high = "#fff394")


# Setting limits explicitly will truncate the scale
# (if any limit is inside the range of the breaks but doesn't
# coincide with any range, it will be rounded with a warning)
v + geom_contour_fill(aes(fill = after_stat(level))) +
  scale_fill_discretised(low = "#a62100", high = "#fff394",
                         limits = c(0.01, 0.028))
#> Warning: User supplied limits don't correspond to valid breaks. [0.01, 0.028] rounded to [0.01, 0.03]


# Or extend it.
v + geom_contour_fill(aes(fill = after_stat(level))) +
  scale_fill_discretised(low = "#a62100", high = "#fff394",
                         limits = c(0, 0.07))


v + geom_contour_fill(aes(fill = after_stat(level))) +
  scale_fill_divergent_discretised(midpoint = 0.02)


# Existing continous scales can be "retrofitted" by changing the `super`
# and `guide` arguments.
v + geom_contour_fill(aes(fill = after_stat(level))) +
    scale_fill_distiller(super = ScaleDiscretised)


# Unequal breaks will, by default, map to unequal spacing in the guide
v + geom_contour_fill(aes(fill = after_stat(level)), breaks = c(0, 0.005, 0.01, 0.02, 0.04)) +
  scale_fill_discretised()


# You can change that by the `even.steps` argument on ggplot2::guide_colorsteps()
v + geom_contour_fill(aes(fill = after_stat(level)), breaks = c(0, 0.005, 0.01, 0.02, 0.04)) +
  scale_fill_discretised(guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE))


```
