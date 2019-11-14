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


# test_that("EOF changes probs", {
#     expect_identical({
#         data(geopotential)
#         round(EOF(gh ~ lat + lon | date, data = geopotential, n = 1, B = 10, probs = c(midd = 0.5))$sdev$midd)
#     }, 1524903)
# })


test_that("EOF works inside data.table", {
    expect_identical(geopotential[, EOF(gh ~ lon + lat | date)$left],
                     EOF(gh ~ lon + lat | date, data = geopotential)$left)
})


test_that("EOF rotates", {
    expect_identical(
        round(EOF(gh ~ lon + lat | date, data = geopotential, n = 1:2, rotate = TRUE)$sdev$sd),
        c(1424982, 542271)
    )
})

test_that("EOF fails gracefully", {
    expect_error(EOF(gh ~ lon + lat | date, data = geopotential, fill = "a"))
    expect_error(EOF(gh ~ lon1 + lat2 | date, data = geopotential),
                 "Columns not found in data: lon1, lat2")
    expect_error(EOF(gh ~ lon | date, data = geopotential),
                 "The formula  gh ~ lon | date  does not identify an unique observation for each cell.")
})

