
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metR <img src="man/figures/logo.png" align="right"/>

[![Build
Status](https://travis-ci.org/eliocamp/metR.svg?branch=master)](https://travis-ci.org/eliocamp/metR)
[![Package
Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Coverage
status](https://codecov.io/gh/eliocamp/metR/branch/master/graph/badge.svg)](https://codecov.io/github/eliocamp/metR/?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/metR)](https://cran.r-project.org/package=metR)

meteR packages several functions and utilities that make R better for
handling meteorological data in the tidy data paradigm. It started
mostly sa a packaging of assorted wrapers and tricks that I wrote for my
day to day work as a researcher in atmospheric sciences. Since then, it
has grown organically and for my own needs and feedback from users.

Conceptually it’s divided into *visualization tools* and *data tools*.
The former are geoms, stats and scales that help with plotting using
[ggplot2](http://ggplot2.tidyverse.org/index.html), such as
`stat_contour_fill()` or `scale_y_level()`, while the later are
functions for common data processing tools in the atmospheric sciences,
such as `Derivate()` or `EOF()`; these are implemented to work in the
[data.table](https://github.com/Rdatatable/data.table/wiki) paradigm,
but also work with regular data frames.

Currently metR is in developement but maturing. Most functions check
arguments and there are some tests. However, some functions might change
it’s interface, and functionality can be moved to other packages, so
please bear that in mind.

## Installation

You can install metR from github with:

``` r
# install.packages("devtools")
devtools::install_github("eliocamp/metR")
```

If you need to read netcdf files, you might need to install the netcdf
and udunits2 libraries. On Ubuntu and it’s derivatives this can be done
by typing

    sudo apt install libnetcdf-dev netcdf-bin libudunits2-dev

## Examples

In this example we easily perform Principal Components Decomposition
(EOF) on monthly geopotential height, then compute the geostrophic wind
associated with this field and plot the field with filled contours and
the wind with streamlines.

``` r
library(metR)
library(data.table)
library(ggplot2)
data(geopotential)
# Use Empirical Orthogonal Functions to compute the Antarctic Oscillation
geopotential <- copy(geopotential)
geopotential[, gh.t.w := Anomaly(gh)*sqrt(cos(lat*pi/180)),
      by = .(lon, lat, month(date))]
aao <- EOF(gh.t.w ~ lat + lon | date, data = geopotential, n = 1)
aao$left[, c("u", "v") := GeostrophicWind(gh.t.w, lon, lat)]

# AAO field
binwidth <- 0.01
ggplot(aao$left, aes(lon, lat, z = gh.t.w)) +
    geom_contour_fill(binwidth = binwidth, xwrap = c(0, 360)) +    # filled contours!
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), 
                    size = 0.4, L = 80, skip = 3, xwrap = c(0, 360)) +
    scale_x_longitude() +
    scale_y_latitude(limits = c(-90, -20)) +
    scale_fill_divergent(name = "AAO pattern", 
                         breaks = MakeBreaks(binwidth),
                         guide = guide_colorstrip()) +
    coord_polar()
```

![](man/figures/field-1.png)<!-- -->

``` r
# AAO signal
ggplot(aao$right, aes(date, gh.t.w)) +
    geom_line() +
    geom_smooth(span = 0.4)
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](man/figures/timeseries-1.png)<!-- -->

You can read more in the vignettes: [Visualization
tools](https://eliocamp.github.io/metR/articles/Visualization-tools.html)
and [Working with
data](https://eliocamp.github.io/metR/articles/Working-with-data.html).
