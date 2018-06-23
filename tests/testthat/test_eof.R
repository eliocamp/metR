library(metR)

context("EOF")

test_that("EOF runs", {
    expect_s3_class({
        data(geopotential)
        EOF(gh ~ lat + lon | date, data = geopotential, n = 1)
    }, "eof")
})

test_that("EOF uses bootstrap", {
    expect_s3_class({
        data(geopotential)
        EOF(gh ~ lat + lon | date, data = geopotential, n = 1, B = 10)
    }, "eof")
})


test_that("EOF changes probs", {
    expect_identical({
        data(geopotential)
        round(EOF(gh ~ lat + lon | date, data = geopotential, n = 1, B = 10, probs = c(midd = 0.5))$sdev$midd)
    }, 2065414)
})


test_that("EOF works with dcast format", {
    expect_identical({
        D <- EOF(lat + lon ~ date, value.var = "gh", data = geopotential, n = 1)
        attr(D, "call") <- NULL
        D
    }, {
        D <- EOF(gh ~ lat + lon | date, data = geopotential, n = 1)
        attr(D, "call") <- NULL
        D})
})


test_that("EOF works inside data.table", {
    expect_identical(geopotential[, EOF(gh ~ lon + lat | date)$left],
                     EOF(gh ~ lon + lat | date, data = geopotential)$left)
})


test_that("EOF rotates", {
    expect_identical(
        round(EOF(gh ~ lon + lat | date, data = geopotential, rotate = TRUE)$sdev$sd),
        2065185
        )
})
