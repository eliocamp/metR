# Impute missing values by linear or constant interpolation

Provides methods for (soft) imputation of missing values.

## Usage

``` r
Impute2D(formula, data = NULL, method = "interpolate")
```

## Arguments

- formula:

  a formula indicating dependent and independent variables (see Details)

- data:

  optional data.frame with the data

- method:

  "interpolate" for interpolation, a numeric for constant imputation or
  a function that takes a vector and returns a number (like
  [mean](https://rdrr.io/r/base/mean.html))

## Details

This is "soft" imputation because the imputed values are not supposed to
be representative of the missing data but just filling for algorithms
that need complete data (in particular, contouring). The method used if
`method = "interpolate"` is to do simple linear interpolation in both
the x and y direction and then average the result.

This is the imputation method used by
[`geom_contour_fill()`](https://eliocamp.github.io/metR/reference/geom_contour_fill.md).
