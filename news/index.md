# Changelog

## metR 0.18.3

### Bug fixes

- Fixed [`I()`](https://rdrr.io/r/base/AsIs.html) notation in
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  subset.

## metR 0.18.2

CRAN release: 2025-09-05

### New features

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now supports cdo operations via the rcdo package. You can pass an rcdo
  operation and it will be executed.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  gains support for reading data from multiple files potentially in
  parallel.

### Bug fixes

- [`ResidLm()`](https://eliocamp.github.io/metR/reference/FitLm.md) now
  returns a vector with NAs.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  with subset used to return the data based on the coordinate closest to
  the subset dimensions. This mean that
  `subset = list(lat = c(-90, -20))` could return data north of 20°S if
  the gridpoint north of 20°S was closer to 20°S than the gridpoint
  South of it. While not a problem in general, this could fail
  catastrophically if the file didn’t have data inside the requested
  range. This now is fixed to that it returns only data inside the
  desired range.

## metR 0.18.1

CRAN release: 2025-05-13

### New features

- [`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md)
  will replace missing values with zeroes.
- New
  [`ParseNetCDFtime()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  to quickly parse netCDF time axes.

### Potentially breaking changes

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now uses the CFtime package to parse times. This shouldn’t lead to any
  braking changes, but times are weird and anything can happen. Please
  report anything amiss.

- Contour breaks are now computed after scale transformation. Previously
  they were computed using the whole dataset, which could lead to
  nonsensical contour breaks if scale limits removed extreme values.

### Other stuff

- Reworked ordering algorithm for tanaka contours.

## metR 0.18.0

CRAN release: 2025-02-24

### New features

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  can now subset based on dimension indices instead of dimension values.
  This makes it possible to read “the first 10 timesteps” or “the last
  10 timesteps” without needing to know which dates they correspond to
  and how many timesteps are in total.

## metR 0.17.0

CRAN release: 2025-01-13

### Bug fixes

- Longitude scales now properly pass the `trans`/`transform` argument.

## metR 0.16.0

CRAN release: 2024-10-14

### New features

- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md)’s `rotate`
  argument now will take a function to apply to the EOF loadings.
  `rotate = TRUE` is still supported but deprecated and will default to
  using `function(x) stats::varimax(x, normalize = FALSE)`.

- Contour functions gain a `proj.latlon` to decide if the projection
  needs to go to latlon coordinates or to projected coordinates.

### Breaking changes

- The
  [`GetSMNData()`](https://eliocamp.github.io/metR/reference/GetSMNData.md)
  function is defunct. The functionality was a hack and hard to maintain
  and it’s also very specific and should be out of scope for a general
  package.
- The `trans` argument of scales has been renamed to `transform`
  following ggplot2’s change.
- [`scale_mag()`](https://eliocamp.github.io/metR/reference/scale_mag.md)
  internals are now much simpler and inherits all the functionality from
  common continuous scales. (thanks
  [@teunbrand](https://github.com/teunbrand) for the suggestion,
  [\#186](https://github.com/eliocamp/metR/issues/186))
- The `step` computed variable in
  [`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md)
  now goes from 0 to the total number of steps instead of from negative
  half to positive half.

## metR 0.15.0

CRAN release: 2024-02-08

### New features

- The contour functions gain a `clip` argument to only show contours in
  an area defined by a polygon.
- The `kriging` argument of the contour functions now can be a numeric
  to control de number of pixels used.
- Documentation of
  [`FitWave()`](https://eliocamp.github.io/metR/reference/waves.md) and
  friends improved
  ([\#184](https://github.com/eliocamp/metR/issues/184),
  [@pascaloettli](https://github.com/pascaloettli)).

### Breaking changes

- The `proj` argument in
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  and friends now operate over the isolines returned by the isoband
  package. This might break code that used a custom function to `proj`.

### Bug Fixes

- Guides are compatible with the new versions of ggplot2
  ([\#117](https://github.com/eliocamp/metR/issues/117) and
  [\#185](https://github.com/eliocamp/metR/issues/185),
  [@teunbrand](https://github.com/teunbrand)).
- Contour functions will work even if OutDec is not “.”.

## metR 0.14.1

CRAN release: 2023-10-30

### Breaking Changes

- The `DivideTimeseries()` function is now defunct.

### New Features

- [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md) gains
  a new `intercept` argument that allows you to remove the intercept
  term that is automatically added.

### Bug Fixes

- Fixes error in
  [`Laplacian()`](https://eliocamp.github.io/metR/reference/Derivate.md)
  when computing the laplacian of a single variable.
  ([\#170](https://github.com/eliocamp/metR/issues/170), Thanks
  [@pascaloettli](https://github.com/pascaloettli))
- Removes dependencies on raster and gdal packages.

## metR 0.14.0

CRAN release: 2023-03-23

### New Features

- Not really a new feature per se, but all geoms now support the new
  linewidth aesthetic.
  [`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md)
  and
  [`geom_shadow()`](https://eliocamp.github.io/metR/reference/geom_relief.md)
  now don’t have a size aesthetic, since they shouldn’t have had it
  anyway.
- The value of the maximum vector magnitude in
  [`scale_mag()`](https://eliocamp.github.io/metR/reference/scale_mag.md)
  is now chosen to be a “pretty” number close to the maximum. This will
  prevent guides with many unnecesary decimal places (thanks,
  [@PanfengZhang](https://github.com/PanfengZhang)
  [\#161](https://github.com/eliocamp/metR/issues/161)).

### Bug Fixes

- Documents the correct default `expand` value in `sale_x_longitude()`
  and friends (thanks,
  [@tamas-ferenci](https://github.com/tamas-ferenci),
  [\#167](https://github.com/eliocamp/metR/issues/167)).
- Fixes inconsistencies in generic methods.

## metR 0.13.0

CRAN release: 2022-10-06

### New features

- The new function
  [`Smooth2D()`](https://eliocamp.github.io/metR/reference/Smooth2D.md)
  smooths a 2D field (hence the name). There are two smoothing methods.
  [`smooth_svd()`](https://eliocamp.github.io/metR/reference/Smooth2D.md)
  computes the SVD of the field and reconstructs it keeping only the
  leading values that ensures a maximum variance lost.
  [`smooth_dct()`](https://eliocamp.github.io/metR/reference/Smooth2D.md)
  computes the Discrete Cosine Transform of the field and sets a
  proportion of the components to zero.

### Bug Fixes

- Fixed a bug in
  [`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md)
  when plotting multiple fields on the same panel.

- Fixes a bug with
  [`GetTopography()`](https://eliocamp.github.io/metR/reference/GetTopography.md)
  on Windows.

- Updates documentation to use valid HTML5 per CRAN’s new format.

- Fixes the “The following aesthetics were dropped during statistical
  transformation” warning for contours.

- Removes
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  file and URL checking because it didn’t work in some cases.
  ([\#164](https://github.com/eliocamp/metR/issues/164), thanks
  [@pascaloettli](https://github.com/pascaloettli))

### Breaking changes

- Due to the udunits2 package being orphaned,
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  not longer uses it to parse dates and times. The homebrewed parser
  might be more limited, and the wild variety of netCDF files in the
  wild makes it hard to fully test. Please report any issues you have!

## metR 0.12.0

CRAN release: 2022-02-15

### New Features

- Adds example of
  [`scale_y_level()`](https://eliocamp.github.io/metR/reference/scale_longitude.md)
  ([@paocorrales](https://github.com/paocorrales),
  [\#153](https://github.com/eliocamp/metR/issues/153)).

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now should parse times correctly even if the use non-standard
  calendars. This now makes udunits2 and PCICt required to parse time.

- Arrays returned by `ReadNetCDF(…, out = "array")` gain a “dimvalues”
  attribute which is analogous to dimnames but has the correct types
  (dates are dates, numerics are numerics, etc…).

- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) gains a
  new `engine` argument to chose the function to compute the singular
  value decomposition.

### Bug Fixes

- Fixed wrong `sdev` component in
  [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) when using
  [`base::svd()`](https://rdrr.io/r/base/svd.html) in some cases.
- [`seasonally()`](https://eliocamp.github.io/metR/reference/season.md)
  now returns a Date object even if the input is datetime. This avoids
  issues when the time component of the input was not all the same.
- Fixed a bug in
  [`ImputeEOF()`](https://eliocamp.github.io/metR/reference/ImputeEOF.md)
  in which the algorithm tried to compute 0 EOFs.

## metR 0.11.0

CRAN release: 2021-09-20

### New Features

- [`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md)
  gains the ability to draw –and leave space for– labels! This is
  finally proper labelling support without having to use a different
  geom
  ([`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)).
  Thanks to the isoband package for this.

- Following [isoband’s naming
  convention](https://isoband.r-lib.org/reference/label_placer.html),
  the family of functions that decide where to place labels has been
  renamed as `label_placer_` and the corresponding argument is now
  `label.placer` . The `label_placement_` family of functions will be
  deprecated in future releases.

## metR 0.10.0

CRAN release: 2021-08-07

### New Features

- discretised scales now work better when passing user-supplied limits.

- New functions to use the International Standard Atmosphere to get
  height from pressure and vice versa, as well as to use as secondary
  axis. See
  [`?standard_atmosphere`](https://eliocamp.github.io/metR/reference/standard_atmosphere.md).

- [`scale_y_level()`](https://eliocamp.github.io/metR/reference/scale_longitude.md)
  and
  [`scale_x_level()`](https://eliocamp.github.io/metR/reference/scale_longitude.md)
  now print more breaks by default. These functions will probably use
  this transformation instead of the reverse log transformation in a
  future release.

- Translation! Thanks to
  [@MichaelChirico](https://github.com/MichaelChirico) priceless
  guidance, metR messages are now translatable and already translated to
  Spanish. If you are using R in a Spanish locale you should be getting
  messages and error in Spanish. Partial translation to Portuguese is
  also included.

- During the process of translating messages, many messages were
  improved and made more consistent.

- New Function
  [`ResidLm()`](https://eliocamp.github.io/metR/reference/FitLm.md) that
  returns the residuals of a linear fit.

- New function
  [`Detrend()`](https://eliocamp.github.io/metR/reference/FitLm.md)
  that, you guessed it, returns a (linearly) detrended version of the
  input vector.

- In
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md),
  the “vars” argument now can take a function.
  ([\#142](https://github.com/eliocamp/metR/issues/142))

- Discretised scales now support user-defined breaks.

### Bugfixes

- [`seasonally()`](https://eliocamp.github.io/metR/reference/season.md)
  result will be on the 15th of the centre month of each season instead
  of on the 1st. This makes the date more representative of the time
  span and also solves a bug in which dates on the 31st would return
  `NA`. This is a **potentially breaking change**.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  doesn’t fail when reading variables with no dimensions (thanks to
  [@paocorrales](https://github.com/paocorrales),
  [\#141](https://github.com/eliocamp/metR/issues/141)).

## metR 0.9.2

CRAN release: 2021-05-20

### Bugfixes

- Fixes a bug in
  [`geom_contour_tanaka()`](https://eliocamp.github.io/metR/reference/geom_contour_tanaka.md)
  in R \>= 4.1.0.

## metR 0.9.1

CRAN release: 2021-02-07

### New Features

- [`as.discretised_scale()`](https://eliocamp.github.io/metR/reference/discretised_scale.md)
  is a quick way of created a discretised version of any continuous
  scale.

- `stroke.colour` is now an accepted aesthetic for
  [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md).

### Bugfixes

- The computations in
  [`MixingRatio()`](https://eliocamp.github.io/metR/reference/thermodynamics.md)
  were wrong. Now they are fixed.

- I really wanted
  [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  to automatically add the arrow legend, but the workarounds I managed
  to write were brittle and couldn’t handle even slight changes in
  people’s code (see <https://github.com/eliocamp/metR/issues/130>). The
  definitive answer is that this is simply not possible due to
  limitations on how ggplot2 works (see
  <https://github.com/tidyverse/ggplot2/issues/4291>). This release,
  then, backtracks those workarounds and tries to accept the things I
  cannot change.

## metR 0.9.0

CRAN release: 2020-11-25

### New Features

- I’m very happy with
  [`discretised_scale()`](https://eliocamp.github.io/metR/reference/discretised_scale.md),
  which is a type of scale which takes a discrete values that are the
  result of some discretisation and treats them as continuous. It’s, in
  a sense, the inverse of the new
  [`ggplot2::binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.html).
  Whereas.
  [`ggplot2::binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.html)
  takes continuous values and then discretises them,
  [`discretised_scale()`](https://eliocamp.github.io/metR/reference/discretised_scale.md)
  takes discrete values which where the result of some discretisation
  procedure (such as the levels of
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)/[`ggplot2::geom_contour_filled()`](https://ggplot2.tidyverse.org/reference/geom_contour.html))
  and allows you to treat them as continuous.

- Related to that,
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  now gains a new computed aesthetic called `level_d`, which is the same
  as `level` but forces ggplot2 to automatically use the new discretised
  scale.

- [`AnchorBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md)
  gains a `bins` argument to mimic the default functionality of
  [`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md).

- New
  [`label_placement_minmax()`](https://eliocamp.github.io/metR/reference/label_placers.md)
  to label contours at the maximum and minimum points of a contour
  (mimicking [isoband’s
  behaviour](https://isoband.r-lib.org/articles/isoband3.html))

- [`geom_contour_tanaka()`](https://eliocamp.github.io/metR/reference/geom_contour_tanaka.md)
  now has a (rather experimental) argument `smooth` which allows to
  smooth the transition between segments.

### Bugfixes

- Fixes error introduced in previous version when
  [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  had mappings other than `dx` and `dy`. (Thanks Santiago!)

- The `level` derived aesthetic from
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  now returns and ordered factor with the correct labels that can be
  interpreted by
  [`ggplot2::guide_colorsteps()`](https://ggplot2.tidyverse.org/reference/guide_coloursteps.html).
  This might a breaking change!

- [`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  lives! The previous release rewrote much of the way
  [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  worked, but I messed up and didn’t realised that the new code had
  broken
  [`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  (to be honest, I’d almost totally forgotten about it :P). (fixes
  [\#126](https://github.com/eliocamp/metR/issues/126), thanks
  [@kongdd](https://github.com/kongdd))

## metR 0.8.0

CRAN release: 2020-10-25

### New features

- [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  placement of labels is completely redesigned. It gains an argument
  `label.placement` which takes a function that is in charge of
  positioning the labels. See
  [`label_placement_flattest()`](https://eliocamp.github.io/metR/reference/label_placers.md)
  for more details on the possible placement methods and how to build
  your own. The default method is `label_placement_flattest`, which
  places the label where the product of the curvature and the angle of
  the contour is minimised. This aims to put labels in a straight
  segment that is also as horizontal as possible. If more than one point
  satisfy this condition, then it chooses the closest to the midpoint.
  This is a **breaking change**, as it will change the label position of
  previous plots.

- The contour functions also gain a `proj` argument. It can be a proj4
  string to project the contours of or an arbitrary function to alter
  the contours after they are computed. This now makes it possible to
  compute contours for data that is on a regular grid on projected
  coordinates but want to plot in lon-lat coordinates (or vice versa).
  Bear in mind that contours that cross the dateline will probably end
  up mangled.

- Contour functions also gain a `kriging` argument. If `TRUE`, will
  perform ordinary kriging to interpolate irregularly placed data into a
  regular grid.

### Bugfixes

- [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md)
  handles `NA`s better.

- `GetSMNData` returns the `date` parameter with the correct time zone.

- [`WaveFlux()`](https://eliocamp.github.io/metR/reference/WaveFlux.md)
  now only returns the value of the horizontal fluxes. That is, it will
  not return lon and lat. **This is a potentially breaking change**.

- The contour family of functions now use `isoband` to compute contours
  (thanks to [@clauswilke](https://github.com/clauswilke) for the
  awesome package) instead of my ugly hack/workaround. As a result,
  contours are faster and much more reliable.

- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) will now
  work even if {irlba} is not installed.

- [`GetTopography()`](https://eliocamp.github.io/metR/reference/GetTopography.md)
  is now updated to the new ETOPO server. It now requires {raster} to
  work.

## metR 0.7.0

CRAN release: 2020-04-10

### New features

- [`FitWave()`](https://eliocamp.github.io/metR/reference/waves.md) and
  related functions return `NA`s when the inputted signal has `NA`s.

- [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md)
  accepts a `weights` argument to perform weighted regression.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now can read files directly from DAP servers and other urls, and
  objects returned by $$ncdf4::nc_{o}pen{()}$$.

### Bugfixes

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  won’t try to parse “time” dimensions that are not dates and will try
  to parse as time all dimensions.

### Breaking changes

- `seasonaly()` is now correctly called
  [`seasonally()`](https://eliocamp.github.io/metR/reference/season.md).
  This proves that you don’t put an ESL person in charge of naming
  stuff.

## metR 0.6.0 - Pileus

CRAN release: 2020-02-10

### New features

- [`EPflux()`](https://eliocamp.github.io/metR/reference/EPflux.md)
  computes Eliassen-Palm fluxes (experimental).

- [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  and
  [`geom_vector()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  should plot faster.

- New functions
  [`is.full_season()`](https://eliocamp.github.io/metR/reference/season.md)
  and `seasonaly()`.

- [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md)
  returns model $r^{2}$ and adjusted $r^{2}$.

- [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md) adds
  names to unnamed terms.

- New function
  [`WaveEnvelope()`](https://eliocamp.github.io/metR/reference/waves.md)
  that computes… the wave envelope.

- [`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  and
  [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  now accept a `global.breaks` argument that controls whether breaks
  should be computed once using the range of the whole dataset or once
  for every grouping (e.g. faceting). `TRUE` (the default) ensures that
  intervals between contours are comparable between panels. Setting it
  to `FALSE` computes contours compatible with `ggplot2::geom_conotur()`
  ([\#109](https://github.com/eliocamp/metR/issues/109), thanks
  [@freeseek](https://github.com/freeseek))

### Bugfixes

- A reworked non-equispaced derivative gives better results in
  [`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md).

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  will not fail if the first variable was called “v” (yeah, I know..
  weird error related to data.table’s non standard evaluation).

- Subsets in
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  won’t fail if no element is named
  ([\#107](https://github.com/eliocamp/metR/issues/107), thanks
  [@m-saenger](https://github.com/m-saenger))

- Fixed bug in
  [`WaveFlux()`](https://eliocamp.github.io/metR/reference/WaveFlux.md)
  ([\#110](https://github.com/eliocamp/metR/issues/110), thanks
  [@salvatirehbein](https://github.com/salvatirehbein))

### Internals

- Cleaned up a lot of dependencies. Some are gone (they were not longer
  needed) and some have been moved to Suggest. Overall metR should now
  be a bit lighter to install.

## metR 0.5.0 - Incus

CRAN release: 2019-11-12

### New features

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  supports using `NA` in subset to refer to max or min value.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)’s
  subset argument supports more complex queries. (see the help section
  at`?ReadNetCDF()`).

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now has a simple date-time parser that is tried if the udunits2
  package is not installed.

- [`GetSMNData()`](https://eliocamp.github.io/metR/reference/GetSMNData.md)
  gains the ability to cache results in a file.

- [`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md)
  now can derive in a non-equispaced grid.

### Bugfixes

- [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  imputation method is fixed for some special cases
  ([\#96](https://github.com/eliocamp/metR/issues/96); thanks
  [@bergmul](https://github.com/bergmul)).

- `predict.eof()` handles complex value svd correctly.

- Accommodates new grid implementation of units
  ([\#105](https://github.com/eliocamp/metR/issues/105) thanks
  [@pmur002](https://github.com/pmur002)).

## metR 0.4.0 - Cumulonimbus

CRAN release: 2019-07-08

### New features

- New
  [`GlanceNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  function that is an alias for `ReadNetCDF(out = "vars")` but now
  prints a human readable summary of the contents of the NetCDF file.

- [`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md)
  now uses 4th order Runge-Kutta instead of plain old Euler. It also
  draws arrows in the middle of the streamline.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  is slightly faster and should use less memory.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  supports more complex subsetting operations now.

- The `df` element returned by
  [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md) now
  has the same length as the rest.

### Bugfixes

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  result will print correctly.

## metR 0.3.0 - Cumulonimbus

CRAN release: 2019-03-12

### New features

- [`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md)
  has a new `baseline` argument.

- New function
  [`Trajectory()`](https://eliocamp.github.io/metR/reference/Trajectory.md)
  that computes trajectories in time-varying velocity fields.

### Bugfixes

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now accepts dates as elements for `subset`.

- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  will read all dates correctly
  ([\#91](https://github.com/eliocamp/metR/issues/91); thanks to
  [@m-saenger](https://github.com/m-saenger)).

### Breaking changes

- The `es` argument from
  [`MixingRatio()`](https://eliocamp.github.io/metR/reference/thermodynamics.md)
  changes to `e`, to be consistent with the rest of the variables.
  Sorry, thermodynamics is not my forte! (thanks
  [@PaoCorrales](https://github.com/PaoCorrales))

- Arrow heads in
  [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  are now scaled correctly in faceted plots (fixes,
  [\#92](https://github.com/eliocamp/metR/issues/92); thanks to
  [@m-saenger](https://github.com/m-saenger))

## metR 0.2.0

CRAN release: 2018-11-19

### Breaking changes

There has been some changes in the interface of some functions for the
sake of consistency.

- In
  [`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md)
  (and it’s derived functions –see what I did there?), the `data`
  argument has been moved back. This is because this function is
  intended to be called inside a `data.table` of `mutate()` call, where
  you don’t need to explicitly specify the data.

- In [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) the
  dcast-style formula interface has been removed. The `data` argument
  was also moved back so you can use the `n` argument more easily
  without naming it.

- [`ImputeEOF()`](https://eliocamp.github.io/metR/reference/ImputeEOF.md)
  follows the same conventions. The dcast-style interface has been
  removed and the `data` argument has been moved after the `max.eof`
  argument.

- `BuildQsWave()` and `FitQsWave()` have been removed and should had
  never even existed.

- The default `skip` argument for
  [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  is now 0.

- Removed `hemisphere` argument from
  [`season()`](https://eliocamp.github.io/metR/reference/season.md)
  since the function returns the trimester so it made no sense.

- Contour functions now compute breaks globally (using all the data)
  instead of per panel. This means default intercomparabilty between
  faceted plots but also a considerable deviation from
  [`ggplot2::geom_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html).

- The `Between` operators are removed since they were already
  implemented in `data.table`.

- The default `geom` for
  [`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md) is
  changed to `point` for consistency with
  [`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)

### Other changes

- Arrows in
  [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  and
  [`geom_vector()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  scale with vector magnitude.
- New geom
  [`geom_streamline()`](https://eliocamp.github.io/metR/reference/geom_streamline.md)
  for visualizing vector fields.
- Utilities
  [`dlon()`](https://eliocamp.github.io/metR/reference/spherical.md),
  [`dlat()`](https://eliocamp.github.io/metR/reference/spherical.md),
  [`dx()`](https://eliocamp.github.io/metR/reference/spherical.md),
  [`dy()`](https://eliocamp.github.io/metR/reference/spherical.md) for
  converting physical units into spherical units.
- New geom
  [`geom_contour_tanaka()`](https://eliocamp.github.io/metR/reference/geom_contour_tanaka.md)
  that plots illuminated contours.
- New function
  [`Interpolate()`](https://eliocamp.github.io/metR/reference/Interpolate.md)
  for bilinear interpolation.
- Fixed bug in
  [`FitWave()`](https://eliocamp.github.io/metR/reference/waves.md) with
  wavenumber 0. Now it returns the mean.
- [`FitWave()`](https://eliocamp.github.io/metR/reference/waves.md) runs
  slightly faster and
  [`BuildWave()`](https://eliocamp.github.io/metR/reference/waves.md)
  runs much faster.
- Removed `GeomContourFill` object since it was just a polygon.
- The results from
  [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) now use
  factors instead of numbers to identify each PC
- New scale
  [`scale_mag()`](https://eliocamp.github.io/metR/reference/scale_mag.md)
  and guide `guide_vector()` for controlling and communicating the scale
  of vectors. These are highly experimental and **will** change in the
  future, but provide some very needed functionality so I decided to
  export them as they are.
- [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  gains new `pivot` argument to control point of rotation and
  `preserve.dir` to tell if angle should be preserved.
- [`stat_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  and
  [`stat_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md)
  print a warning when no contours can be made.
- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) now
  supports estimation of confidence intervals via bootstrap.
- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) supports
  varimax rotation. Rotated components are labelled accordingly.
- [`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md)
  is much faster now (but see package `rayshader`).
- New
  [`geom_shadow()`](https://eliocamp.github.io/metR/reference/geom_relief.md)
  for casting shadows in topographic maps.
- Contour calculations in `StatContour2` are
  [memoised](https://github.com/r-lib/memoise) so they are only computed
  once even adding several layers with the same contours
  (`geom_contour() + geom_text_contour()`) or running the same plot
  while tweaking it’s appearance.
- New [`FitLm()`](https://eliocamp.github.io/metR/reference/FitLm.md)
  for getting fast estimates of linear fits.
- New
  [`FilterWave()`](https://eliocamp.github.io/metR/reference/waves.md)
  for filtering waves.
- Circular dimensions had quite a ride during this development process.
  [`RepeatCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md)
  was deprecated in favour of
  [`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md)
  which is more robust, and then
  [`WrapCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md)
  was deprecated in favour of the `ggperiodic` package.
- The way that
  [`stat_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md),
  [`stat_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  and `geom_streamlines()` handle circular dimensions has changed. Now
  you need to use `xwrap`/`ywrap` to explicitly set the domain. This
  makes the implementation more robust and also allow to easily wrap to
  an arbitrary domain.
- After that change, the wrapping functionality has moved to
  `ggperiodic`, which can handle this stuff better. The above mentioned
  arguments still work but are not documented and will be deprecated.
- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) gains a
  `fill` argument for sparse-ish data.
- [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  now has the ability to draw a stroke around text.
- [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  now can impute missing values with bivariate interpolation.
- `BuildField()` actually renamed
  [`BuildWave()`](https://eliocamp.github.io/metR/reference/waves.md)
  (which should’ve happen in the previous release according to this
  News).
- New function
  [`as.path()`](https://eliocamp.github.io/metR/reference/as.path.md)
  and added functionality to
  [`Interpolate()`](https://eliocamp.github.io/metR/reference/Interpolate.md)
  that allows to interpolate values along a path of locations.
- New function
  [`Impute2D()`](https://eliocamp.github.io/metR/reference/Impute2D.md)
  which is an exported version of the method used by
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  to (soft) impute missing values.
- `subset` argument in
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now accepts character vectors for time components.

## metR 0.1.0

- New function:
  [`GetTopography()`](https://eliocamp.github.io/metR/reference/GetTopography.md)
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now can output arrays and vectors.
- Changed name.
- New website (<https://eliocamp.github.io/metR/>) with documentation.
  Thanks to `pkgdown`.
- New functions:
  [`coriolis()`](https://eliocamp.github.io/metR/reference/coriolis.md)
  and
  [`coriolis.dy()`](https://eliocamp.github.io/metR/reference/coriolis.md).
- Faster
  [`Anomaly()`](https://eliocamp.github.io/metR/reference/Anomaly.md)
  (can’t believe I did that).
- [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) principal
  components are numeric instead of “PC1”, etc…
- For consistency with
  [`stat_contour()`](https://ggplot2.tidyverse.org/reference/geom_contour.html)
  created
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  which works just like
  [`stat_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md).
- Added
  [`stat_na()`](https://eliocamp.github.io/metR/reference/stat_na.md)
  for easy masking of `NA` values.
- [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  now is
  [`geom_vector()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  with better documentation and more parameters and
  [`geom_arrow()`](https://eliocamp.github.io/metR/reference/geom_arrow.md)
  is a new geom for arrows that preserve direction.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now handles more date formats.
- New functions
  [`Laplacian()`](https://eliocamp.github.io/metR/reference/Derivate.md),
  `Divercence()` and
  [`Vorticity()`](https://eliocamp.github.io/metR/reference/Derivate.md)
- `DerivatePhysical()` is removed since it was made redundant by the
  extended functionality in
  [`Derivate()`](https://eliocamp.github.io/metR/reference/Derivate.md)
- New functions related to several physical processes (see ?physics).
- New
  [`geom_text_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  and
  [`geom_label_contour()`](https://eliocamp.github.io/metR/reference/geom_text_contour.md)
  for labelling contours.
- New function
  [`GeostrophicWind()`](https://eliocamp.github.io/metR/reference/GeostrophicWind.md).
- Fixed? weird bug with
  [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  and `as.POSIXct`.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  now supports time-zones via de `udunits2` package.
- Fixed bad polygon ordering and extra polygon in
  [`stat_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md).
- New functions
  [`MakeBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md)
  and
  [`AnchorBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md).
- New guide.
  [`guide_colorstrip()`](https://eliocamp.github.io/metR/reference/guide_colourstrip.md)
  displays discretized values of a continuous colour or fill scale.
- Fix for unconnected contours in
  [`stat_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
- New
  [`geom_relief()`](https://eliocamp.github.io/metR/reference/geom_relief.md)
  for generating relief shading.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  gains a `subset` argument for subsetting data.  
- [`AnchorBreaks()`](https://eliocamp.github.io/metR/reference/MakeBreaks.md)
  is a new way of generating breaks.
- New
  [`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md)
  that takes a function as argument in `breaks` and
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  also does the same.
- New
  [`stat_subset()`](https://eliocamp.github.io/metR/reference/stat_subset.md)
  that makes subsetting data on the fly easier.
- [`ReadNetCDF()`](https://eliocamp.github.io/metR/reference/ReadNetCDF.md)
  can return a keyed data.table.
- `FitQsWave()` and `BuildQsWave()` renamed to
  [`FitWave()`](https://eliocamp.github.io/metR/reference/waves.md) and
  [`BuildWave()`](https://eliocamp.github.io/metR/reference/waves.md),
  respectively.
- [`GetSMNData()`](https://eliocamp.github.io/metR/reference/GetSMNData.md)
  is updated to include the new SMN website and data types.
- [`geom_contour2()`](https://eliocamp.github.io/metR/reference/geom_contour2.md)
  and
  [`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md)
  gain a `circular` argument to specify a circular dimension.
- [`RepeatCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md)
  renamed to
  [`RepeatCircular()`](https://eliocamp.github.io/metR/reference/WrapCircular.md).
- [`LonLabel()`](https://eliocamp.github.io/metR/reference/map_labels.md)
  and
  [`LatLabel()`](https://eliocamp.github.io/metR/reference/map_labels.md)
  aid labelling latitude and longitude.
- Changed interface of
  [`EOF()`](https://eliocamp.github.io/metR/reference/EOF.md) and
  [`ImputeEOF()`](https://eliocamp.github.io/metR/reference/ImputeEOF.md).
