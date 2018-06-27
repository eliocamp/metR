library(metR)
library(vdiffr)

context("contour_fill")
basic_fill <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    geom_contour_fill(aes(z = gh))
basic_fill_stat <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
    stat_contour_fill(aes(z = gh))

test_that("geom_contour_fill works", {
    expect_doppelganger("contour_fill-base", basic_fill)
    expect_doppelganger("contour_fill-base", basic_fill_stat)
})

