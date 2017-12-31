# metR 0.0.9101
- New function: `GetTopography()`
- `ReadNetCDF()` now can output arrays and vectors.
- Changed name. 
- New website ([https://eliocamp.github.io/metR/](https://eliocamp.github.io/metR/)) with documentation. Thanks to `pkgdown`.
- New functions: `coriolis()` and `coriolis.dy()`.
- Faster `Anomaly()` (can't believe I did that).
- `EOF()` principal componets are numeric instead of "PC1", etc...
- For consistency with `stat_contour()` created `geom_contour_fill()` which 
works just like `stat_contour_fill()`. 
- Added `stat_na()` for easy masking of `NA` values. 
- `geom_arrow()` now is `geom_vector()` with better documentation and more 
parameters. 
- `geom_arrow()` is a new geom for arrows that preserve direction. 
- `ReadNetCDF()` now handles more date formats.
- New functions `Laplacian()`, `Divercence()` and `Vorticity()`
- `DerivatePhysical()` is removed since it was made redundant by the extended
functionality in `Derivate()`
- New function `IdealGas()` for ideal gas calculations. 
- New `geom_text_contour()` and `geom_label_contour()` for labeling contours. 
- New function `GeostrophicWind()`.
- Fixed? werid bug with `ReadNetCDF()` and `as.POSIXct`.
- `ReadNetCDF()` now supports timezones via de `udunits2` package.
- Fixed bad polygon ordering and extra polygon in `stat_contour_fill()`.


# meteoR 0.0.9100
- Full (not perfect) documentation.
- Ready for serious testing.
