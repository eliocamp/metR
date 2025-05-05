
context("EOF")
data(geopotential)
test_that("EOF runs", {
    expect_s3_class({
        EOF(gh ~ lat + lon | date, data = geopotential, n = 1)
    }, "eof")
})

test_that("EOF, returns correct PCs", {
    expect_equal(nrow(EOF(gh ~ lat + lon | date, data = geopotential, n = 2)$sdev), 1)
})


test_that("EOF uses bootstrap", {
    expect_s3_class({
        data(geopotential)
        EOF(gh ~ lat + lon | date, data = geopotential, n = 1, B = 10)
    }, "eof")
})



test_that("can use differnet engine", {
    set.seed(40)
    # with this seed, the base::svd result has a different sign.
    skip_if_not_installed("irlba")
    expect_equal(
        EOF(gh ~ lat + lon | date, data = geopotential, engine = base::svd)$left[, gh := -gh][],
        EOF(gh ~ lat + lon | date, data = geopotential, n = 1)$left
    )
})

test_that("EOF works inside data.table", {
    expect_equal(geopotential[, EOF(gh ~ lon + lat | date)$left],
                 EOF(gh ~ lon + lat | date, data = geopotential)$left)
})


test_that("EOF rotates", {
    expect_identical(
        round(EOF(gh ~ lon + lat | date, data = geopotential, n = 1:2, rotate = function(x) stats::varimax(x, normalize = FALSE))$sdev$sd),
        c(1424982, 542271)
    )

    expect_equal(
        EOF(gh ~ lon + lat | date, data = geopotential, n = 1:2,
            rotate = function(x) stats::varimax(x, normalize = FALSE))$sdev$sd,
        expect_warning(EOF(gh ~ lon + lat | date, data = geopotential, n = 1:2,
                           rotate = TRUE)$sdev$sd, "deprecated"))
})



test_that("EOF fails gracefully", {
    expect_error(EOF(gh ~ lon + lat | date, data = geopotential, fill = "a"))
    expect_error(EOF(gh ~ lon1 + lat2 | date, data = geopotential),
                 "Columns not found in data: lon1, lat2")
    expect_error(EOF(gh ~ lon | date, data = geopotential),
                 "The formula gh ~ lon | date  does not identify an unique observation for each cell.")
})

test_that("eof methods", {
    eof <- EOF(gh ~ lat + lon | date, data = geopotential, n = 1:5,
               engine = base::svd)   # need to force this engine so that predict is exact

    eof_12 <- cut(eof, 1:2)
    expect_s3_class(eof_12, "eof")
    expect_equal(as.character(unique(eof_12$left$PC)), c("PC1", "PC2"))
    expect_equal(as.character(unique(eof_12$right$PC)), c("PC1", "PC2"))
    expect_equal(as.character(unique(eof_12$sdev$PC)), c("PC1", "PC2"))

    expect_true(inherits(screeplot(eof), "gg"))
    expect_true(inherits(autoplot(eof), "gg"))



    eof_all <- EOF(gh ~ lat + lon | date, data = geopotential, n = NULL,
                   engine = base::svd)
    expect_equal(geopotential[, .(lat, lon, date, gh)][order(lat, lon, date)],
                 predict(eof_all)[order(lat, lon, date)])

    expect_equal(predict(eof_all, n = 1:5), predict(eof))


    expect_known_output(summary(eof), file = "eof_summary")

})
