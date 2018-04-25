# metR 0.1.9000
- Arrows in `geom_arrow()` and `geom_vector()` scale with vector magnitude. 
- New geom `geom_streamline()` for visualizing vector fields.
- Utilities `dlon()`, `dlat()`, `dx()`, `dy()` for converting physical units into 
spherical units. 
- New geom `geom_contour_tanaka()` that plots illuminated contours. 
- New function `Interpolate()` for bilinear interpolation.
- Fixed bug in `FitWave()` with wavenumber 0. Now it rerturns the mean. 
- `FitWave()` runs slightly faster and `BuildField()` runs much faster. 
- Removed `GeomContourFill` object since it was just a polygon. 
- The results from `EOF()` now use factors instead of numbers to identify each
PC
- New scale `scale_vector()` and guide `guide_vector()` for controlling and 
communicating the scale of vectors. 
- `geom_arrow()` gains new `pivot` agument to control point of rotation and 
`preserve.dir` to tell if angle should be preserved. 
- `stat_contour_fill()` and `stat_contour2()` print a warning when no contours 
can be made.

# metR 0.1
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
