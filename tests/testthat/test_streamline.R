library(metR)
library(ggplot2)

context("Streamline")
geo <- geopotential[date == date[1]]
geo[, c("u", "v") := GeostrophicWind(gh, lon, lat)]

test_that("Streamline works", {
    expect_equal(
        nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                               geom_streamline(aes(dx = u, dy = v)))$data[[1]]),
        3497)
})

test_that("Streamline wraps in x amd y", {
    expect_equal(nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                                       geom_streamline(aes(dx = u, dy = v),
                                                       xwrap = c(0, 360)))$data[[1]]),
                 3675
    )
    expect_equal(nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                                       geom_streamline(aes(dx = u, dy = v),
                                                       ywrap = c(-90, -20)))$data[[1]]),
                 4340)
})

