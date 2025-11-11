# Scale for vector magnitudes

Allows to control the size of the arrows in
[geom_arrow](https://eliocamp.github.io/metR/reference/geom_arrow.md).
Highly experimental.

## Usage

``` r
scale_mag(
  name = ggplot2::waiver(),
  n.breaks = 1,
  breaks = ggplot2::waiver(),
  oob = no_censor,
  ...
)

scale_mag_continuous(
  name = ggplot2::waiver(),
  n.breaks = 1,
  breaks = ggplot2::waiver(),
  oob = no_censor,
  ...
)
```

## Arguments

- name:

  The name of the scale. Used as the axis or legend title. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), the
  default, the name of the scale is taken from the first mapping used
  for that aesthetic. If `NULL`, the legend title will be omitted.

- n.breaks:

  An integer guiding the number of major breaks. The algorithm may
  choose a slightly different number to ensure nice break labels. Will
  only have an effect if `breaks = waiver()`. Use `NULL` to use the
  default number of breaks given by the transformation.

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

- ...:

  Other arguments passed on to `scale_(x|y)_continuous()`

## Examples

``` r
library(ggplot2)
g <- ggplot(seals, aes(long, lat)) +
    geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)

g + scale_mag("Seals velocity")


g + scale_mag("Seals velocity", limits = c(0, 1))

```
