# metR 0.12.0

## New Features

-   Adds example of `scale_y_level()` (\@paocorrales, #153).

-   `ReadNetCDF()` now should parse times correctly even if the use non-standard calendars.
    This now makes udunits2 and PCICt required to parse time.

-   Arrays returned by `ReadNetCDF(…, out = "array")` gain a "dimvalues" attribute which is analogous to dimnames but has the correct types (dates are dates, numerics are numerics, etc...).

-   `EOF()` gains a new `engine` argument to chose the function to compute the singular value decomposition.

## Bug Fixes

-   Fixed wrong `sdev` component in `EOF()` when using `base::svd()` in some cases.
-   `seasonally()` now returns a Date object even if the input is datetime. This avoids issues when the time component of the input was not all the same.
-   Fixed a bug in `ImputeEOF()` in which the algorithm tried to compute 0 EOFs.

# metR 0.11.0

## New Features

-   `geom_contour2()` gains the ability to draw --and leave space for-- labels!
    This is finally proper labelling support without having to use a different geom (`geom_text_contour()`).
    Thanks to the isoband package for this.

-   Following [isoband's naming convention](https://wilkelab.org/isoband/reference/label_placer.html), the family of functions that decide where to place labels has been renamed as `label_placer_` and the corresponding argument is now `label.placer` .
    The `label_placement_` family of functions will be deprecated in future releases.

# metR 0.10.0

## New Features

-   discretised scales now work better when passing user-supplied limits.

-   New functions to use the International Standard Atmosphere to get height from pressure and vice versa, as well as to use as secondary axis.
    See `?standard_atmosphere`.

-   `scale_y_level()` and `scale_x_level()` now print more breaks by default.
    These functions will probably use this transformation instead of the reverse log transformation in a future release.

-   Translation!
    Thanks to @MichaelChirico priceless guidance, metR messages are now translatable and already translated to Spanish.
    If you are using R in a Spanish locale you should be getting messages and error in Spanish.
    Partial translation to Portuguese is also included.

-   During the process of translating messages, many messages were improved and made more consistent.

-   New Function `ResidLm()` that returns the residuals of a linear fit.

-   New function `Detrend()` that, you guessed it, returns a (linearly) detrended version of the input vector.

-   In `ReadNetCDF()`, the "vars" argument now can take a function.
    (#142)

-   Discretised scales now support user-defined breaks.

## Bugfixes

-   `seasonally()` result will be on the 15th of the centre month of each season instead of on the 1st. This makes the date more representative of the time span and also solves a bug in which dates on the 31st would return `NA`. This is a **potentially breaking change**.
-   `ReadNetCDF()` doesn't fail when reading variables with no dimensions (thanks to @paocorrales, #141).

# metR 0.9.2

## Bugfixes

-   Fixes a bug in `geom_contour_tanaka()` in R >= 4.1.0.

# metR 0.9.1

## New Features

-   `as.discretised_scale()` is a quick way of created a discretised version of any continuous scale.

-   `stroke.colour` is now an accepted aesthetic for `geom_text_contour()`.

## Bugfixes

-   The computations in `MixingRatio()` were wrong.
    Now they are fixed.

-   I really wanted `geom_arrow()` to automatically add the arrow legend, but the workarounds I managed to write were brittle and couldn't handle even slight changes in people's code (see <https://github.com/eliocamp/metR/issues/130>).
    The definitive answer is that this is simply not possible due to limitations on how ggplot2 works (see <https://github.com/tidyverse/ggplot2/issues/4291>).
    This release, then, backtracks those workarounds and tries to accept the things I cannot change.

# metR 0.9.0

## New Features

-   I'm very happy with `discretised_scale()`, which is a type of scale which takes a discrete values that are the result of some discretisation and treats them as continuous.
    It's, in a sense, the inverse of the new `ggplot2::binned_scale()`.
    Whereas.
    `ggplot2::binned_scale()` takes continuous values and then discretises them, `discretised_scale()` takes discrete values which where the result of some discretisation procedure (such as the levels of `geom_contour_fill()`/`ggplot2::geom_contour_filled()`) and allows you to treat them as continuous.

-   Related to that, `geom_contour_fill()` now gains a new computed aesthetic called `level_d`, which is the same as `level` but forces ggplot2 to automatically use the new discretised scale.

-   `AnchorBreaks()` gains a `bins` argument to mimic the default functionality of `MakeBreaks()`.

-   New `label_placement_minmax()` to label contours at the maximum and minimum points of a contour (mimicking [isoband's behaviour](https://wilkelab.org/isoband/articles/isoband3.html))

-   `geom_contour_tanaka()` now has a (rather experimental) argument `smooth` which allows to smooth the transition between segments.

## Bugfixes

-   Fixes error introduced in previous version when `geom_arrow()` had mappings other than `dx` and `dy`.
    (Thanks Santiago!)

-   The `level` derived aesthetic from `geom_contour_fill()` now returns and ordered factor with the correct labels that can be interpreted by `ggplot2::guide_colorsteps()`.
    This might a breaking change!

-   `geom_label_contour()` lives!
    The previous release rewrote much of the way `geom_text_contour()` worked, but I messed up and didn't realised that the new code had broken `geom_label_contour()` (to be honest, I'd almost totally forgotten about it :P).
    (fixes #126, thanks @kongdd)

# metR 0.8.0

## New features

-   `geom_text_contour()` placement of labels is completely redesigned.
    It gains an argument `label.placement` which takes a function that is in charge of positioning the labels.
    See `label_placement_flattest()` for more details on the possible placement methods and how to build your own.
    The default method is `label_placement_flattest`, which places the label where the product of the curvature and the angle of the contour is minimised.
    This aims to put labels in a straight segment that is also as horizontal as possible.
    If more than one point satisfy this condition, then it chooses the closest to the midpoint.
    This is a **breaking change**, as it will change the label position of previous plots.

-   The contour functions also gain a `proj` argument.
    It can be a proj4 string to project the contours of or an arbitrary function to alter the contours after they are computed.
    This now makes it possible to compute contours for data that is on a regular grid on projected coordinates but want to plot in lon-lat coordinates (or vice versa).
    Bear in mind that contours that cross the dateline will probably end up mangled.

-   Contour functions also gain a `kriging` argument.
    If `TRUE`, will perform ordinary kriging to interpolate irregularly placed data into a regular grid.

## Bugfixes

-   `FitLm()` handles `NA`s better.

-   `GetSMNData` returns the `date` parameter with the correct time zone.

-   `WaveFlux()` now only returns the value of the horizontal fluxes.
    That is, it will not return lon and lat.
    **This is a potentially breaking change**.

-   The contour family of functions now use `isoband` to compute contours (thanks to @clauswilke for the awesome package) instead of my ugly hack/workaround.
    As a result, contours are faster and much more reliable.

-   `EOF()` will now work even if {irlba} is not installed.

-   `GetTopography()` is now updated to the new ETOPO server.
    It now requires {raster} to work.

# metR 0.7.0

## New features

-   `FitWave()` and related functions return `NA`s when the inputted signal has `NA`s.

-   `FitLm()` accepts a `weights` argument to perform weighted regression.

-   `ReadNetCDF()` now can read files directly from DAP servers and other urls, and objects returned by $$ncdf4::nc_open()$$.

## Bugfixes

-   `ReadNetCDF()` won't try to parse "time" dimensions that are not dates and will try to parse as time all dimensions.

## Breaking changes

-   `seasonaly()` is now correctly called `seasonally()`. This proves that you don't put an ESL person in charge of naming stuff.

# metR 0.6.0 - Pileus

## New features

-   `EPflux()` computes Eliassen-Palm fluxes (experimental).

-   `geom_arrow()` and `geom_vector()` should plot faster.

-   New functions `is.full_season()` and `seasonaly()`.

-   `FitLm()` returns model $r^2$ and adjusted $r^2$.

-   `FitLm()` adds names to unnamed terms.

-   New function `WaveEnvelope()` that computes... the wave envelope.

-   `geom_contour2()`, `geom_contour_fill()` and `geom_text_contour()` now accept a `global.breaks` argument that controls whether breaks should be computed once using the range of the whole dataset or once for every grouping (e.g. faceting).
    `TRUE` (the default) ensures that intervals between contours are comparable between panels.
    Setting it to `FALSE` computes contours compatible with `ggplot2::geom_conotur()` (#109, thanks @freeseek)

## Bugfixes

-   A reworked non-equispaced derivative gives better results in `Derivate()`.

-   `ReadNetCDF()` will not fail if the first variable was called "v" (yeah, I know.. weird error related to data.table's non standard evaluation).

-   Subsets in `ReadNetCDF()` won't fail if no element is named (#107, thanks @m-saenger)

-   Fixed bug in `WaveFlux()` (#110, thanks @salvatirehbein)

## Internals

-   Cleaned up a lot of dependencies. Some are gone (they were not longer needed) and some have been moved to Suggest. Overall metR should now be a bit lighter to install.

# metR 0.5.0 - Incus

## New features

-   `ReadNetCDF()` supports using `NA` in subset to refer to max or min value.

-   `ReadNetCDF()`'s subset argument supports more complex queries.
    (see the help section at`?ReadNetCDF()`).

-   `ReadNetCDF()` now has a simple date-time parser that is tried if the udunits2 package is not installed.

-   `GetSMNData()` gains the ability to cache results in a file.

-   `Derivate()` now can derive in a non-equispaced grid.

## Bugfixes

-   `geom_contour_fill()` imputation method is fixed for some special cases (#96; thanks @bergmul).

-   `predict.eof()` handles complex value svd correctly.

-   Accommodates new grid implementation of units (#105 thanks @pmur002).

# metR 0.4.0 - Cumulonimbus

## New features

-   New `GlanceNetCDF()` function that is an alias for `ReadNetCDF(out = "vars")` but now prints a human readable summary of the contents of the NetCDF file.

-   `geom_streamline()` now uses 4th order Runge-Kutta instead of plain old Euler.
    It also draws arrows in the middle of the streamline.

-   `ReadNetCDF()` is slightly faster and should use less memory.

-   `ReadNetCDF()` supports more complex subsetting operations now.

-   The `df` element returned by `FitLm()` now has the same length as the rest.

## Bugfixes

-   `ReadNetCDF()` result will print correctly.

# metR 0.3.0 - Cumulonimbus

## New features

-   `Anomaly()` has a new `baseline` argument.

-   New function `Trajectory()` that computes trajectories in time-varying velocity fields.

## Bugfixes

-   `ReadNetCDF()` now accepts dates as elements for `subset`.

-   `ReadNetCDF()` will read all dates correctly (#91; thanks to @m-saenger).

## Breaking changes

-   The `es` argument from `MixingRatio()` changes to `e`, to be consistent with the rest of the variables.
    Sorry, thermodynamics is not my forte!
    (thanks @PaoCorrales)

-   Arrow heads in `geom_arrow()` are now scaled correctly in faceted plots (fixes, #92; thanks to @m-saenger)

# metR 0.2.0

## Breaking changes

There has been some changes in the interface of some functions for the sake of consistency.

-   In `Derivate()` (and it's derived functions --see what I did there?), the `data` argument has been moved back.
    This is because this function is intended to be called inside a `data.table` of `mutate()` call, where you don't need to explicitly specify the data.

-   In `EOF()` the dcast-style formula interface has been removed.
    The `data` argument was also moved back so you can use the `n` argument more easily without naming it.

-   `ImputeEOF()` follows the same conventions.
    The dcast-style interface has been removed and the `data` argument has been moved after the `max.eof` argument.

-   `BuildQsWave()` and `FitQsWave()` have been removed and should had never even existed.

-   The default `skip` argument for `geom_text_contour()` is now 0.

-   Removed `hemisphere` argument from `season()` since the function returns the trimester so it made no sense.

-   Contour functions now compute breaks globally (using all the data) instead of per panel.
    This means default intercomparabilty between faceted plots but also a considerable deviation from `ggplot2::geom_contour()`.

-   The `Between` operators are removed since they were already implemented in `data.table`.

-   The default `geom` for `stat_na()` is changed to `point` for consistency with `stat_subset()`

## Other changes

-   Arrows in `geom_arrow()` and `geom_vector()` scale with vector magnitude.
-   New geom `geom_streamline()` for visualizing vector fields.
-   Utilities `dlon()`, `dlat()`, `dx()`, `dy()` for converting physical units into spherical units.
-   New geom `geom_contour_tanaka()` that plots illuminated contours.
-   New function `Interpolate()` for bilinear interpolation.
-   Fixed bug in `FitWave()` with wavenumber 0. Now it returns the mean.
-   `FitWave()` runs slightly faster and `BuildWave()` runs much faster.
-   Removed `GeomContourFill` object since it was just a polygon.
-   The results from `EOF()` now use factors instead of numbers to identify each PC
-   New scale `scale_mag()` and guide `guide_vector()` for controlling and communicating the scale of vectors. These are highly experimental and **will** change in the future, but provide some very needed functionality so I decided to export them as they are.
-   `geom_arrow()` gains new `pivot` argument to control point of rotation and `preserve.dir` to tell if angle should be preserved.
-   `stat_contour_fill()` and `stat_contour2()` print a warning when no contours can be made.
-   `EOF()` now supports estimation of confidence intervals via bootstrap.
-   `EOF()` supports varimax rotation. Rotated components are labelled accordingly.
-   `geom_relief()` is much faster now (but see package `rayshader`).
-   New `geom_shadow()` for casting shadows in topographic maps.
-   Contour calculations in `StatContour2` are [memoised](https://github.com/r-lib/memoise) so they are only computed once even adding several layers with the same contours (`geom_contour() + geom_text_contour()`) or running the same plot while tweaking it's appearance.
-   New `FitLm()` for getting fast estimates of linear fits.
-   New `FilterWave()` for filtering waves.
-   Circular dimensions had quite a ride during this development process. `RepeatCircular()` was deprecated in favour of `WrapCircular()` which is more robust, and then `WrapCircular()` was deprecated in favour of the `ggperiodic` package.
-   The way that `stat_contour2()`, `stat_contour_fill()` and `geom_streamlines()` handle circular dimensions has changed. Now you need to use `xwrap`/`ywrap` to explicitly set the domain. This makes the implementation more robust and also allow to easily wrap to an arbitrary domain.
-   After that change, the wrapping functionality has moved to `ggperiodic`, which can handle this stuff better. The above mentioned arguments still work but are not documented and will be deprecated.
-   `EOF()` gains a `fill` argument for sparse-ish data.
-   `geom_text_contour()` now has the ability to draw a stroke around text.
-   `geom_contour_fill()` now can impute missing values with bivariate interpolation.
-   `BuildField()` actually renamed `BuildWave()` (which should've happen in the previous release according to this News).
-   New function `as.path()` and added functionality to `Interpolate()` that allows to interpolate values along a path of locations.
-   New function `Impute2D()` which is an exported version of the method used by `geom_contour_fill()` to (soft) impute missing values.
-   `subset` argument in `ReadNetCDF()` now accepts character vectors for time components.

# metR 0.1.0

-   New function: `GetTopography()`
-   `ReadNetCDF()` now can output arrays and vectors.
-   Changed name.
-   New website (<https://eliocamp.github.io/metR/>) with documentation. Thanks to `pkgdown`.
-   New functions: `coriolis()` and `coriolis.dy()`.
-   Faster `Anomaly()` (can't believe I did that).
-   `EOF()` principal components are numeric instead of "PC1", etc...
-   For consistency with `stat_contour()` created `geom_contour_fill()` which works just like `stat_contour_fill()`.
-   Added `stat_na()` for easy masking of `NA` values.
-   `geom_arrow()` now is `geom_vector()` with better documentation and more parameters and `geom_arrow()` is a new geom for arrows that preserve direction.
-   `ReadNetCDF()` now handles more date formats.
-   New functions `Laplacian()`, `Divercence()` and `Vorticity()`
-   `DerivatePhysical()` is removed since it was made redundant by the extended functionality in `Derivate()`
-   New functions related to several physical processes (see ?physics).
-   New `geom_text_contour()` and `geom_label_contour()` for labelling contours.
-   New function `GeostrophicWind()`.
-   Fixed? weird bug with `ReadNetCDF()` and `as.POSIXct`.
-   `ReadNetCDF()` now supports time-zones via de `udunits2` package.
-   Fixed bad polygon ordering and extra polygon in `stat_contour_fill()`.
-   New functions `MakeBreaks()` and `AnchorBreaks()`.
-   New guide. `guide_colorstrip()` displays discretized values of a continuous colour or fill scale.
-   Fix for unconnected contours in `stat_contour_fill()`
-   New `geom_relief()` for generating relief shading.
-   `ReadNetCDF()` gains a `subset` argument for subsetting data.\
-   `AnchorBreaks()` is a new way of generating breaks.
-   New `geom_contour2()` that takes a function as argument in `breaks` and `geom_contour_fill()` also does the same.
-   New `stat_subset()` that makes subsetting data on the fly easier.
-   `ReadNetCDF()` can return a keyed data.table.
-   `FitQsWave()` and `BuildQsWave()` renamed to `FitWave()` and `BuildWave()`, respectively.
-   `GetSMNData()` is updated to include the new SMN website and data types.
-   `geom_contour2()` and `geom_contour_fill()` gain a `circular` argument to specify a circular dimension.
-   `RepeatCircular()` renamed to `RepeatCircular()`.
-   `LonLabel()` and `LatLabel()` aid labelling latitude and longitude.
-   Changed interface of `EOF()` and `ImputeEOF()`.

# meteoR 0.0.9100

-   Full (not perfect) documentation.
-   Ready for serious testing.
