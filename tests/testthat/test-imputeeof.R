library(metR)

context("Impute EOF")

library(data.table)
data(geopotential)
geopotential <- copy(geopotential)
geopotential[, gh.t := Anomaly(gh), by = .(lat, lon, month(date))]

# Add gaps to field
geopotential[, gh.gap := gh.t]
set.seed(42)
geopotential[sample(1:.N, .N*0.3), gh.gap := NA]
geopotential[, gh.impute := ImputeEOF(gh.gap ~ lat + lon | date, max.eof = 5,
                                      max.iter = 2000)]
test_that("Returns a vector in the correct order", {
    expect_equal(geopotential[!is.na(gh.gap), all(gh.gap - gh.impute == 0)],
                 TRUE)
})

test_that("Imputes reasonable values", {
    expect_equal(sign(geopotential[, cor(gh, gh.impute)]),
                 1)
})


M <- metR:::.tidy2matrix(geopotential, lon + lat ~ date, value.var = "gh.gap")$matrix
M.imputed <- ImputeEOF(data = M, max.eof = 5)

test_that("Works with matrix format", {
    expect_equal(is.matrix(M.imputed), TRUE)
    expect_equal(dim(M), dim(M.imputed))
    expect_equal(all((M - M.imputed)[!is.na(M)] == 0), TRUE)
})

geopotential[, gh.impute := ImputeEOF(gh.gap ~ lat + lon | date, max.eof = 5, max.iter = 2000)]
test_that("returns ok if there's no missing values", {
    expect_warning(gh <<- geopotential[, ImputeEOF(gh ~ lat + lon | date, max.eof = 5,
                                                   max.iter = 2000)],
                   "no missing values")
    expect_equal(gh,
                 geopotential$gh)
})

