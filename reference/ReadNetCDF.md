# Read NetCDF files.

Using the
[`ncdf4-package`](https://rdrr.io/pkg/ncdf4/man/ncdf4-package.html)
package, it reads a NetCDF file. The advantage over using
[`ncvar_get`](https://rdrr.io/pkg/ncdf4/man/ncvar_get.html) is that the
output is a tidy data.table with proper dimensions.

## Usage

``` r
ReadNetCDF(
  file,
  vars = NULL,
  out = c("data.frame", "vector", "array"),
  subset = NULL,
  key = FALSE
)

ParseNetCDFtime(time)

OpenNetCDF(files)

GlanceNetCDF(file, ...)
```

## Arguments

- file:

  source to read from. Must be one of:

  - A string representing a local file(s) with read access.

  - A string representing a URL readable by
    [`ncdf4::nc_open()`](https://rdrr.io/pkg/ncdf4/man/nc_open.html).
    (this includes DAP urls).

  - A netcdf object returned by
    [`ncdf4::nc_open()`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or
    a list of such. (Use `OpenNetCDF()` as sa helper function.)

- vars:

  one of:

  - `NULL`: reads all variables.

  - a character vector with the name of the variables to read.

  - a function that takes a vector with all the variables and returns
    either a character vector with the name of variables to read or a
    numeric/logical vector that indicates a subset of variables.

- out:

  character indicating the type of output desired

- subset:

  a list of subsetting objects. See below.

- key:

  if `TRUE`, returns a data.table keyed by the dimensions of the data.

- time:

  the time definition. Can be accessed using GlanceNetCDF.

- files:

  vector of files to open.

- ...:

  in `GlanceNetCDF()`, ignored. Is there for convenience so that a call
  to `ReadNetCDF()` can be also valid for `GlanceNetCDF()`.

## Value

The return format is specified by `out`. It can be a data table in which
each column is a variable and each row, an observation; an array with
named dimensions; or a vector. Since it's possible to return multiple
arrays or vectors (one for each variable), for consistency the return
type is always a list. Either of these two options are much faster than
the first since the most time consuming part is the melting of the array
returned by
[ncdf4::ncvar_get](https://rdrr.io/pkg/ncdf4/man/ncvar_get.html).
`out = "vector"` is particularly useful for adding new variables to an
existing data frame with the same dimensions.

When not all variables specified in `vars` have the same number of
dimensions, the shorter variables will be recycled. E.g. if reading a 3D
pressure field and a 2D surface temperature field, the latter will be
turned into a 3D field with the same values in each missing dimension.

`GlanceNetCDF()` returns a list of variables and dimensions included in
the file with a nice printing method.

## Multifle datasets

`ReadNetCDF()` has rudimentary support for multifile datasets. If the
`file` argument is a vector of files, then the function will be applied
to each of them with the same arguments using
[`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html)
and the result concatenated.\`

Array output is not supported in this case, since it's not clear how
each result should be combined.

If a file doesn't have the data included in a subset, then it won't be
read (but the metadata will need to be processed). To read multiple
times from the same multifle dataset, using `OpenNetCDF()` to first open
all the connections might speed things up. Still, for now coordinates
are read and parsed every time, so the process can be slow for dataset
with a very large number of files.

## Subsetting

In the most basic form, `subset` will be a named list whose names must
match the dimensions specified in the NetCDF file and each element must
be a vector whose range defines a contiguous subset of data. You don't
need to provide and exact range that matches the actual gridpoints of
the file; the closest gridpoint will be selected. Furthermore, you can
use `NA` to refer to the existing minimum or maximum.

So, if you want to get Southern Hemisphere data from the from a file
that defines latitude as `lat`, then you can use:

    subset = list(lat = -90:0)

To use dimension indices instead of values, wrap the expression in
[`base::I()`](https://rdrr.io/r/base/AsIs.html). For example to read the
first 10 timesteps of a file:

    subset = list(time = I(1, 10))

Negative indices are interpreted as starting from the end. So to read
the last 10 timesteps of a file:

    subset = list(time = I(-10, 0))

More complex subsetting operations are supported. If you want to read
non-contiguous chunks of data, you can specify each chunk into a list
inside `subset`. For example this subset

    subset = list(list(lat = -90:-70, lon = 0:60),
                  list(lat = 70:90, lon = 300:360))

will return two contiguous chunks: one on the South-West corner and one
on the North-East corner. Alternatively, if you want to get the four
corners that are combination of those two conditions,

    subset = list(lat = list(-90:-70, 70:90),
                  lon = list(0:60, 300:360))

Both operations can be mixed together. So for example this

    subset = list(list(lat = -90:-70,
                       lon = 0:60),
                  time = list(c("2000-01-01", "2000-12-31"),
                              c("2010-01-01", "2010-12-31")))

returns one spatial chunk for each of two temporal chunks.

The general idea is that named elements define 'global' subsets ranges
that will be applied to every other subset, while each unnamed element
define one contiguous chunk. In the above example, `time` defines two
temporal ranges that every subset of data will have.

The above example, then, is equivalent to

    subset = list(list(lat = -90:-70,
                       lon = 0:60,
                       time = c("2000-01-01", "2000-12-31")),
                  list(lat = -90:-70,
                       lon = 0:60,
                       time = c("2010-01-01", "2010-12-31")))

but demands much less typing.

## Examples

``` r
file <- system.file("extdata", "temperature.nc", package = "metR")
# Get a list of variables.
variables <- GlanceNetCDF(file)
print(variables)
#> ----- Variables ----- 
#> air:
#>     mean Daily Air temperature in degK
#>     Dimensions: lon by lat by level by time
#> 
#> 
#> ----- Dimensions ----- 
#>   time: 1 values from 2010-07-09 to 2010-07-09 
#>   level: 17 values from 10 to 1000 millibar
#>   lat: 73 values from -90 to 90 degrees_north
#>   lon: 144 values from 0 to 357.5 degrees_east

# The object returned by GlanceNetCDF is a list with lots
# of information
str(variables)
#> List of 2
#>  $ vars:List of 1
#>   ..$ air:List of 22
#>   .. ..$ id                :List of 5
#>   .. .. ..$ id         : num 0
#>   .. .. ..$ group_index: num -1
#>   .. .. ..$ group_id   : int 65536
#>   .. .. ..$ list_index : num 1
#>   .. .. ..$ isdimvar   : logi FALSE
#>   .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. ..$ name              : chr "air"
#>   .. ..$ ndims             : int 4
#>   .. ..$ natts             : int 14
#>   .. ..$ size              : int [1:4] 144 73 17 1
#>   .. ..$ dimids            : int [1:4] 3 2 1 0
#>   .. ..$ prec              : chr "float"
#>   .. ..$ units             : chr "degK"
#>   .. ..$ longname          : chr "mean Daily Air temperature"
#>   .. ..$ group_index       : int 1
#>   .. ..$ chunksizes        : int [1:4] 144 73 17 1
#>   .. ..$ storage           : num 2
#>   .. ..$ shuffle           : int 1
#>   .. ..$ compression       : int 2
#>   .. ..$ dims              : list()
#>   .. ..$ dim               :List of 4
#>   .. .. ..$ :List of 10
#>   .. .. .. ..$ name         : chr "lon"
#>   .. .. .. ..$ len          : int 144
#>   .. .. .. ..$ unlim        : logi FALSE
#>   .. .. .. ..$ group_index  : int 1
#>   .. .. .. ..$ group_id     : int 65536
#>   .. .. .. ..$ id           : int 3
#>   .. .. .. ..$ dimvarid     :List of 5
#>   .. .. .. .. ..$ id         : int 3
#>   .. .. .. .. ..$ group_index: int 1
#>   .. .. .. .. ..$ group_id   : int 65536
#>   .. .. .. .. ..$ list_index : num -1
#>   .. .. .. .. ..$ isdimvar   : logi TRUE
#>   .. .. .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. .. .. ..$ units        : chr "degrees_east"
#>   .. .. .. ..$ vals         : num [1:144(1d)] 0 2.5 5 7.5 10 12.5 15 17.5 20 22.5 ...
#>   .. .. .. ..$ create_dimvar: logi TRUE
#>   .. .. .. ..- attr(*, "class")= chr "ncdim4"
#>   .. .. ..$ :List of 10
#>   .. .. .. ..$ name         : chr "lat"
#>   .. .. .. ..$ len          : int 73
#>   .. .. .. ..$ unlim        : logi FALSE
#>   .. .. .. ..$ group_index  : int 1
#>   .. .. .. ..$ group_id     : int 65536
#>   .. .. .. ..$ id           : int 2
#>   .. .. .. ..$ dimvarid     :List of 5
#>   .. .. .. .. ..$ id         : int 1
#>   .. .. .. .. ..$ group_index: int 1
#>   .. .. .. .. ..$ group_id   : int 65536
#>   .. .. .. .. ..$ list_index : num -1
#>   .. .. .. .. ..$ isdimvar   : logi TRUE
#>   .. .. .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. .. .. ..$ units        : chr "degrees_north"
#>   .. .. .. ..$ vals         : num [1:73(1d)] 90 87.5 85 82.5 80 77.5 75 72.5 70 67.5 ...
#>   .. .. .. ..$ create_dimvar: logi TRUE
#>   .. .. .. ..- attr(*, "class")= chr "ncdim4"
#>   .. .. ..$ :List of 10
#>   .. .. .. ..$ name         : chr "level"
#>   .. .. .. ..$ len          : int 17
#>   .. .. .. ..$ unlim        : logi FALSE
#>   .. .. .. ..$ group_index  : int 1
#>   .. .. .. ..$ group_id     : int 65536
#>   .. .. .. ..$ id           : int 1
#>   .. .. .. ..$ dimvarid     :List of 5
#>   .. .. .. .. ..$ id         : int 2
#>   .. .. .. .. ..$ group_index: int 1
#>   .. .. .. .. ..$ group_id   : int 65536
#>   .. .. .. .. ..$ list_index : num -1
#>   .. .. .. .. ..$ isdimvar   : logi TRUE
#>   .. .. .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. .. .. ..$ units        : chr "millibar"
#>   .. .. .. ..$ vals         : num [1:17(1d)] 1000 925 850 700 600 500 400 300 250 200 ...
#>   .. .. .. ..$ create_dimvar: logi TRUE
#>   .. .. .. ..- attr(*, "class")= chr "ncdim4"
#>   .. .. ..$ :List of 10
#>   .. .. .. ..$ name         : chr "time"
#>   .. .. .. ..$ len          : int 1
#>   .. .. .. ..$ unlim        : logi TRUE
#>   .. .. .. ..$ group_index  : int 1
#>   .. .. .. ..$ group_id     : int 65536
#>   .. .. .. ..$ id           : int 0
#>   .. .. .. ..$ dimvarid     :List of 5
#>   .. .. .. .. ..$ id         : int 4
#>   .. .. .. .. ..$ group_index: int 1
#>   .. .. .. .. ..$ group_id   : int 65536
#>   .. .. .. .. ..$ list_index : num -1
#>   .. .. .. .. ..$ isdimvar   : logi TRUE
#>   .. .. .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. .. .. ..$ units        : chr "hours since 1800-01-01 00:00:0.0"
#>   .. .. .. ..$ vals         : num [1(1d)] 1845360
#>   .. .. .. ..$ create_dimvar: logi TRUE
#>   .. .. .. ..- attr(*, "class")= chr "ncdim4"
#>   .. ..$ varsize           : int [1:4] 144 73 17 1
#>   .. ..$ unlim             : logi TRUE
#>   .. ..$ make_missing_value: logi TRUE
#>   .. ..$ missval           : num -9.97e+36
#>   .. ..$ hasAddOffset      : logi FALSE
#>   .. ..$ hasScaleFact      : logi FALSE
#>   .. ..- attr(*, "class")= chr "ncvar4"
#>  $ dims:List of 4
#>   ..$ time :List of 10
#>   .. ..$ name         : chr "time"
#>   .. ..$ len          : int 1
#>   .. ..$ unlim        : logi TRUE
#>   .. ..$ group_index  : int 1
#>   .. ..$ group_id     : int 65536
#>   .. ..$ id           : int 0
#>   .. ..$ dimvarid     :List of 5
#>   .. .. ..$ id         : int 4
#>   .. .. ..$ group_index: int 1
#>   .. .. ..$ group_id   : int 65536
#>   .. .. ..$ list_index : num -1
#>   .. .. ..$ isdimvar   : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. ..$ units        : chr "hours since 1800-01-01 00:00:0.0"
#>   .. ..$ vals         : num [1(1d)] 1845360
#>   .. ..$ create_dimvar: logi TRUE
#>   .. ..- attr(*, "class")= chr "ncdim4"
#>   ..$ level:List of 10
#>   .. ..$ name         : chr "level"
#>   .. ..$ len          : int 17
#>   .. ..$ unlim        : logi FALSE
#>   .. ..$ group_index  : int 1
#>   .. ..$ group_id     : int 65536
#>   .. ..$ id           : int 1
#>   .. ..$ dimvarid     :List of 5
#>   .. .. ..$ id         : int 2
#>   .. .. ..$ group_index: int 1
#>   .. .. ..$ group_id   : int 65536
#>   .. .. ..$ list_index : num -1
#>   .. .. ..$ isdimvar   : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. ..$ units        : chr "millibar"
#>   .. ..$ vals         : num [1:17(1d)] 1000 925 850 700 600 500 400 300 250 200 ...
#>   .. ..$ create_dimvar: logi TRUE
#>   .. ..- attr(*, "class")= chr "ncdim4"
#>   ..$ lat  :List of 10
#>   .. ..$ name         : chr "lat"
#>   .. ..$ len          : int 73
#>   .. ..$ unlim        : logi FALSE
#>   .. ..$ group_index  : int 1
#>   .. ..$ group_id     : int 65536
#>   .. ..$ id           : int 2
#>   .. ..$ dimvarid     :List of 5
#>   .. .. ..$ id         : int 1
#>   .. .. ..$ group_index: int 1
#>   .. .. ..$ group_id   : int 65536
#>   .. .. ..$ list_index : num -1
#>   .. .. ..$ isdimvar   : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. ..$ units        : chr "degrees_north"
#>   .. ..$ vals         : num [1:73(1d)] 90 87.5 85 82.5 80 77.5 75 72.5 70 67.5 ...
#>   .. ..$ create_dimvar: logi TRUE
#>   .. ..- attr(*, "class")= chr "ncdim4"
#>   ..$ lon  :List of 10
#>   .. ..$ name         : chr "lon"
#>   .. ..$ len          : int 144
#>   .. ..$ unlim        : logi FALSE
#>   .. ..$ group_index  : int 1
#>   .. ..$ group_id     : int 65536
#>   .. ..$ id           : int 3
#>   .. ..$ dimvarid     :List of 5
#>   .. .. ..$ id         : int 3
#>   .. .. ..$ group_index: int 1
#>   .. .. ..$ group_id   : int 65536
#>   .. .. ..$ list_index : num -1
#>   .. .. ..$ isdimvar   : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "ncid4"
#>   .. ..$ units        : chr "degrees_east"
#>   .. ..$ vals         : num [1:144(1d)] 0 2.5 5 7.5 10 12.5 15 17.5 20 22.5 ...
#>   .. ..$ create_dimvar: logi TRUE
#>   .. ..- attr(*, "class")= chr "ncdim4"
#>  - attr(*, "class")= chr [1:2] "nc_glance" "list"

# Read only the first one, with name "var".
field <- ReadNetCDF(file, vars = c(var = names(variables$vars[1])))
# Add a new variable.
# ¡Make sure it's on the same exact grid!
field[, var2 := ReadNetCDF(file, out = "vector")]
#>               time level   lat   lon    var   var2
#>             <POSc> <num> <num> <num>  <num>  <num>
#>      1: 2010-07-09  1000    90   0.0 274.87 274.87
#>      2: 2010-07-09  1000    90   2.5 274.87 274.87
#>      3: 2010-07-09  1000    90   5.0 274.87 274.87
#>      4: 2010-07-09  1000    90   7.5 274.87 274.87
#>      5: 2010-07-09  1000    90  10.0 274.87 274.87
#>     ---                                           
#> 178700: 2010-07-09    10   -90 347.5 188.25 188.25
#> 178701: 2010-07-09    10   -90 350.0 188.25 188.25
#> 178702: 2010-07-09    10   -90 352.5 188.25 188.25
#> 178703: 2010-07-09    10   -90 355.0 188.25 188.25
#> 178704: 2010-07-09    10   -90 357.5 188.25 188.25

if (FALSE) { # \dontrun{
# Using a DAP url
url <- "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.GMAO/.GEOS_V2p1/.hindcast/.ua/dods"
field <- ReadNetCDF(url, subset = list(M = 1,
                                       P = 10,
                                       S = "1999-01-01"))

# In this case, opening the netcdf file takes a non-neglible
# amount of time. So if you want to iterate over many dimensions,
# then it's more efficient to open the file first and then read it.

ncfile <- ncdf4::nc_open(url)
field <- ReadNetCDF(ncfile, subset = list(M = 1,
                                       P = 10,
                                       S = "1999-01-01"))


# Using a function in `vars` to read all variables that
# start with "radar_".
ReadNetCDF(radar_file, vars = function(x) startsWith(x, "radar_"))

} # }
```
