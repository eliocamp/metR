
context("ReadNetCDF")
skip_if_not_installed(c("ncdf4", "CFtime"))
file <- system.file("extdata", "temperature.nc", package = "metR")
test_that("returns a data.table", {
    expect_s3_class(ReadNetCDF(file),
                        "data.table")
})

test_that("GlanceNetCDF prints nicely", {
    expect_known_output(print(GlanceNetCDF(file)), "GlanceNetCDF")

})

test_that("subsetting works", {
    r <- ReadNetCDF(file,
                    subset = list(lat = -90:20))

    expect_equal(range(r$lat), c(-90, 20))


    s <-  list(
        list(lat = -90:-70, lon = 0:60),
        list(lat = 70:90, lon = 300:360)
    )
    expect_known_value(ReadNetCDF(file, subset = s), "readnetcdf_unnamed_subset")

})

test_that("naming works", {
    expect_true({
        t <- ReadNetCDF(file,
                        vars = c(air2 = "air"))
        "air2" %in% colnames(t)})
})

test_that("different outs work", {
    expect_s3_class(ReadNetCDF(file),
                    "data.table")
    expect_is(ReadNetCDF(file, out = "array")[[1]], "array")
    expect_is(ReadNetCDF(file, out = "vector")[[1]], "numeric")
})


test_that("time dimension without 'since' works", {
    file <- "weird_datesmall.nc"
    read <- ReadNetCDF(file)
    expect_true(!is.null(read$time))
})

test_that("can parse calendar correcly", {
    file <- "calendar.nc4"
    read <- ReadNetCDF(file, vars = "zg")
    expect_true(all(as.POSIXlt(read$time)$mon == 11))
})


test_that("can read from nc_open", {
    nc <- ncdf4::nc_open(file)
    expect_error(ReadNetCDF(nc), NA)
})


test_that("can read from urls", {
    url <- "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.GMAO/.GEOS_V2p1/.hindcast/.ua/dods"
    skip_if_offline()
    skip_on_cran()
    expect_s3_class(GlanceNetCDF(url), "nc_glance")
})



test_that("can read variables with no dimension", {
    # issue #141
    file <- "no-dim.nc"
    expect_equal(ReadNetCDF(file, "projection"), 1)
})

test_that("function in vars works", {
    file <- "weird_datesmall.nc"

    read_benchmark <- ReadNetCDF(file, vars = "o3_conc")
    expect_identical(ReadNetCDF(file, vars = function(x) startsWith(x, "o3")),
                     read_benchmark)
    expect_identical(ReadNetCDF(file, vars = function(x) x[startsWith(x, "o3")]),
                     read_benchmark)

    expect_warning(expect_null(ReadNetCDF(file, vars = function(x) startsWith(x, "o5"))))
    expect_error(ReadNetCDF(file, "o5"))
    expect_error(ReadNetCDF(file, vars = function(x) "o5"))
})
