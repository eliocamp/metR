library(metR)


context("season")
test_that("season assigns season", {
    expect_equal(as.character(season(1)), "DJF")
    expect_equal(as.character(season(1, lang = "es")),
                 "DEF")
    expect_equal(as.character(season(c(1, 3))),
                 c("DJF", "MAM"))
    expect_equal(levels(season(1)),
                 c("DJF", "MAM", "JJA", "SON"))
})




context("MaskLand")
test_that("Water is still water", {
    skip_if_not_installed("maps")

    expect_equal(MaskLand(120, 14), FALSE)
    expect_equal(MaskLand(360-58, -34), TRUE)
    expect_equal(MaskLand(-58, -34, wrap = c(-180, 180)), TRUE)
})


test_that("MaskLand respects boundaries", {
    skip_if_not_installed("maps")
    expect_equal(MaskLand(361, 15), MaskLand(1, 15))
    expect_equal(MaskLand(0, -91), NA)
    expect_equal(MaskLand(0, 91), NA)
})

test_that("MaskLand respects order", {
    skip_if_not_installed("maps")
    expect_equal(MaskLand(c(120, 360-58), c(14, -34)), c(FALSE, TRUE))
})
