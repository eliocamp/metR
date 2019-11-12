# metR 0.5.0 - Incus

## New features

- `ReadNetCDF()` suports using `NA` in subset to refer to max or min value. 

- `ReadNetCDF()`'s subset argument supports more complex queries. (see the help 
section at`?ReadNetCDF()`). 

- `ReadNetCDF()` now has a simple date-time parser that is tried if the udunits2 package
is not installed. 

- `GetSMNData()` gains the ability to cache results in a file. 

- `Derivate()` now can derive in a non-equispaced grid. 

## Bugfixes

- `geom_contour_fill()` imputation method is fixed for some special cases (#96; thanks @bergmul).

- `predict.eof()` handles complex value svd correctly.

- Accommodates new grid implementation of units (#105 thanks @pmur002).

# metR 0.4.0 - Cumulonimbus

## New features

- New `GlanceNetCDF()` function that is an alias for `ReadNetCDF(out = "vars")` but now prints a human readable summary of the contents of the NetCDF file.

- `geom_streamline()` now uses 4th order Runge-Kutta insteaf of plain old Euler. It also draws arrows in the middle of the streamline. 

- `ReadNetCDF()` is slightly faster and should use less memory. 

- `ReadNetCDF()` supports more complex subsetting operations now. 

- The `df` element returned by `FitLm()` now has the same length as the rest. 
    
## Bugfixes

- `ReadNetCDF()` result will print correctly. 

# metR 0.3.0 - Cumulonimbus

## New features

- `Anomaly()` has a new `baseline` argument. 

- New function `Trajectory()` that computes trajectories in time-varying 
velocity fields. 

##  Bugfixes 

- `ReadNetCDF()` now accepts dates as elements for `subset`.

- `ReadNetCDF()` will read all dates correctly (#91; thanks to @m-saenger). 

## Breaking changes

- The `es` argument from `MixingRatio()` changes to `e`, to be consistent with
the rest of the variables. Sorry, thermodynamics is not my forte! (thanks @PaoCorrales)

- Arrow heads in `geom_arrow()` are now scaled correctly in faceted plots (fixes, #92; thanks to @m-saenger)

# metR 0.2.0

## Breaking changes

There has been some changes in the interface of some functions for the sake of 
consistency.

- In `Derivate()` (and it's derivated functions --see what I did there?), the
`data` argument has been moved back. This is because this function is intended to
be called inside a `data.table` of `mutate()` call, where you don't need to 
explicitly specify the data.

- In `EOF()` the dcast-style formula interface has been removed. The `data` argument
was also moved back so you can use the `n` argument more easily without naming it. 

- `ImputeEOF()` follows the same conventions. The dcast-style interface has been 
removed and the `data` argument has been moved after the `max.eof` argument.

- `BuildQsWave()` and `FitQsWave()` have been removed and should had never even
existed. 

- The default `skip` argument for `geom_text_contour()` is now 0.

- Removed `hemisphere` argument from `season()` since the function returns the
trimester so it made no sense. 

- Contour functions now compute breaks globaly (using all the data) instead of
per panel. This means default intercomparabilty between facetwed plots but also 
a considerable deviation from `ggplot2::geom_contour()`. 

- The `Between` operators are removed since they were already implemented in 
`data.table`.

- The default `geom` for `stat_na()` is changed to `point` for consistency with 
`stat_subset()`

## Other changes

- Arrows in `geom_arrow()` and `geom_vector()` scale with vector magnitude. 
- New geom `geom_streamline()` for visualizing vector fields.
- Utilities `dlon()`, `dlat()`, `dx()`, `dy()` for converting physical units into 
spherical units. 
- New geom `geom_contour_tanaka()` that plots illuminated contours. 
- New function `Interpolate()` for bilinear interpolation.
- Fixed bug in `FitWave()` with wavenumber 0. Now it rerturns the mean. 
- `FitWave()` runs slightly faster and `BuildWave()` runs much faster. 
- Removed `GeomContourFill` object since it was just a polygon. 
- The results from `EOF()` now use factors instead of numbers to identify each
PC
- New scale `scale_mag()` and guide `guide_vector()` for controlling and 
communicating the scale of vectors. These are highly experimental and **will** 
change in the future, but provide some very needed functionality so I decided to
export them as they are. 
- `geom_arrow()` gains new `pivot` agument to control point of rotation and 
`preserve.dir` to tell if angle should be preserved. 
- `stat_contour_fill()` and `stat_contour2()` print a warning when no contours 
can be made.
- `EOF()` now supports estimation of confidence intervals via bootstrap.
- `EOF()` supports varimax rotation. Rotated components are labeled accordingly. 
- `geom_relief()` is much faster now (but see package `rayshader`).
- New `geom_shadow()` for casting shadows in topographic maps.
- Contour calculations in `StatContour2` are 
[memoised](https://github.com/r-lib/memoise) so they are
only computed once even adding several layers with the same contours 
(`geom_contour() + geom_text_contour()`) or running the same plot while tweaking
it's appearance.
- New `FitLm()` for getting fast estimates of linear fits. 
- New `FilterWave()` for filtering waves. 
- Circular dimensions had quite a ride during this developement process. `RepeatCircular()` was deprecated in favour of `WrapCircular()` which is more
robust, and then `WrapCircular()` was deprecated in favour of the `ggperiodic` package.
- The way that `stat_contour2()`, `stat_contour_fill()` and `geom_streamlines()`
handle circular dimensions has changed. Now you need to use `xwrap`/`ywrap` to 
explicitly set the domain. This makes the implementation more robust and also allow to easily wrap to an arbitrary domain.
- After that change, the wrapping functionallity has moved to `ggperiodic`, which 
can handle this stuff better. The above mentioned arguments still work but are not 
documented and will be deprecated. 
- `EOF()` gains a `fill` argument for sparse-ish data. 
- `geom_text_contour()` now has the ability to draw a stroke around text. 
- `geom_contour_fill()` now can impute missing values with bivariate interpolation. 
- `BuildField()` actually renamed `BuildWave()` (which should've happen in the previous
release acording to this News).
- New function `as.path()` and added functionality to `Interpolate()` that allows
to interpolate values along a path of locations.
- New function `Impute2D()` which is an exported version of the method used by 
`geom_contour_fill()` to (soft) impute missing values.
- `subset` argument in `ReadNetCDF()` now accepts character vectors for time 
components. 

# metR 0.1.0

- New function: `GetTopography()`
- `ReadNetCDF()` now can output arrays and vectors.
- Changed name. 
- New website ([https://eliocamp.github.io/metR/](https://eliocamp.github.io/metR/)) with documentation. Thanks to `pkgdown`.
- New functions: `coriolis()` and `coriolis.dy()`.
- Faster `Anomaly()` (can't believe I did that).
- `EOF()` principal components are numeric instead of "PC1", etc...
- For consistency with `stat_contour()` created `geom_contour_fill()` which 
works just like `stat_contour_fill()`. 
- Added `stat_na()` for easy masking of `NA` values. 
- `geom_arrow()` now is `geom_vector()` with better documentation and more 
parameters and `geom_arrow()` is a new geom for arrows that preserve direction. 
- `ReadNetCDF()` now handles more date formats.
- New functions `Laplacian()`, `Divercence()` and `Vorticity()`
- `DerivatePhysical()` is removed since it was made redundant by the extended
functionality in `Derivate()`
- New functions related to several physical processes (see ?physics).
- New `geom_text_contour()` and `geom_label_contour()` for labeling contours. 
- New function `GeostrophicWind()`.
- Fixed? werid bug with `ReadNetCDF()` and `as.POSIXct`.
- `ReadNetCDF()` now supports timezones via de `udunits2` package.
- Fixed bad polygon ordering and extra polygon in `stat_contour_fill()`.
- New functions `MakeBreaks()` and `AnchorBreaks()`.
- New guide. `guide_colorstrip()` displays discretized values of a continuous color 
or fill scale. 
- Fix for unconnected contours in `stat_contour_fill()`
- New `geom_relief()` for generating relief shading. 
- `ReadNetCDF()` gains a `subset` argument for subseting data.  
- `AnchorBreaks()` is a new way of generating breaks.
- New `geom_contour2()` that takes a function as argument in `breaks` and 
`geom_contour_fill()` also does the same. 
- New `stat_subset()` that makes subsetting data on the fly easier. 
- `ReadNetCDF()` can return a keyed data.table. 
- `FitQsWave()` and `BuildQsWave()` renamed to `FitWave()` and `BuildWave()`, 
respectively.
- `GetSMNData()` is updated to include the new SMN website and data types. 
- `geom_contour2()` and `geom_contour_fill()` gain a `circular` argument to specify
a circular dimension.
- `RepeatCircular()` renamed to `RepeatCircular()`.
- `LonLabel()` and `LatLabel()` aid labeling latitude and longitude. 
- Changed interface of `EOF()` and `ImputeEOF()`.

# meteoR 0.0.9100
- Full (not perfect) documentation.
- Ready for serious testing.
