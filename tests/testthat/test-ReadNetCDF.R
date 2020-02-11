
context("ReadNetCDF")

file <- system.file("extdata", "temperature.nc", package = "metR")
test_that("returns a data.table", {
    expect_known_output(ReadNetCDF(file),
                        "readnetcdf_default")
})

test_that("subsetting works", {
    expect_known_output(ReadNetCDF(file,
                                   subset = list(lat = -90:20)),
                        "readnetcdf_subset")


    s <-  list(
        list(lat = -90:-70, lon = 0:60),
        list(lat = 70:90, lon = 300:360)
    )
    expect_known_output(ReadNetCDF(file, subset = s), "readnetcdf_unnamed_subset")

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
