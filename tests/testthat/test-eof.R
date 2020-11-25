
context("EOF")
data(geopotential)
test_that("EOF runs", {
    expect_s3_class({
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

test_that("eof methods", {
    eof <- EOF(gh ~ lat + lon | date, data = geopotential, n = 1:5)

    eof_12 <- cut(eof, 1:2)
    expect_s3_class(eof_12, "eof")
    expect_equal(as.character(unique(eof_12$left$PC)), c("PC1", "PC2"))
    expect_equal(as.character(unique(eof_12$right$PC)), c("PC1", "PC2"))
    expect_equal(as.character(unique(eof_12$sdev$PC)), c("PC1", "PC2"))

    expect_true(inherits(screeplot(eof), "gg"))
    expect_true(inherits(autoplot(eof), "gg"))



    eof_all <- EOF(gh ~ lat + lon | date, data = geopotential, n = NULL)
    expect_equal(geopotential[, .(lat, lon, date, gh)][order(lat, lon, date)],
                 predict(eof_all)[order(lat, lon, date)])

    expect_equal(predict(eof_all, n = 1:5), predict(eof))

    expect_known_output(print(eof), file = "eof_print")

    expect_known_output(summary(eof), file = "eof_summary")

})
