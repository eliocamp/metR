# Divergent colour scales

Wrapper around ggplot's
[`scale_colour_gradient2`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
with inverted defaults of `high` and `low`.

## Usage

``` r
scale_colour_divergent(
  ...,
  low = scales::muted("blue"),
  mid = "white",
  high = scales::muted("red"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar"
)

scale_color_divergent(
  ...,
  low = scales::muted("blue"),
  mid = "white",
  high = scales::muted("red"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar"
)

scale_fill_divergent(
  ...,
  low = scales::muted("blue"),
  mid = "white",
  high = scales::muted("red"),
  midpoint = 0,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar"
)
```

## Arguments

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

- mid:

  colour for mid point

- midpoint:

  The midpoint (in data value) of the diverging scale. Defaults to 0.

- space:

  colour space in which to calculate gradient. Must be "Lab" - other
  values are deprecated.

- na.value:

  Colour to use for missing values

- guide:

  Type of legend. Use `"colourbar"` for continuous colour bar, or
  `"legend"` for discrete colour legend.

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
[`scale_longitude`](https://eliocamp.github.io/metR/reference/scale_longitude.md),
[`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md),
[`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

## Examples

``` r
library(ggplot2)
ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
  geom_contour(aes(color = after_stat(level))) +
  scale_colour_divergent(midpoint = 130)
```
