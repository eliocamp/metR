library(metR)
library(ggplot2)
library(data.table)
# library(vdiffr)

context("Streamline")
geo <- geopotential[date == date[1]]
geo[, c("u", "v") := GeostrophicWind(gh, lon, lat)]

test_that("Streamline works", {
    expect_equal(
        nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                               geom_streamline(aes(dx = u, dy = v)))$data[[1]]),
        3511)
    expect_equal(
        nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                              stat_streamline(aes(dx = u, dy = v)))$data[[1]]),
        3511)
})

test_that("Streamline wraps in x amd y", {
    expect_equal(nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                                       geom_streamline(aes(dx = u, dy = v),
                                                       xwrap = c(0, 360)))$data[[1]]),
                 3725
    )
    expect_equal(nrow(ggplot_build(ggplot(geo, aes(lon, lat)) +
                                       geom_streamline(aes(dx = u, dy = v),
                                                       ywrap = c(-90, -20)))$data[[1]]),
                 4340)
})

data <- as.data.table(expand.grid(x = 1:50, y = 1:50))
data[, c("dx", "dy") := .(-sign(x - mean(x)), -sign(y - mean(y)))]
test_that("Streamline builds the grid correctly", {
    expect_equal({
        d <- ggplot_build(ggplot(data, aes(x, y)) +
                         geom_streamline(aes(dx = dx, dy = dy),
                                         nx = 10, ny = 10, jitter = 0, S = 2))$data[[1]]
        nrow(subset(d, step == 0))
    }, 100)
    expect_equal({
        d <- ggplot_build(ggplot(data, aes(x, y)) +
                              geom_streamline(aes(dx = dx, dy = dy), skip = 10,
                                              jitter = 0, S = 2))$data[[1]]
        nrow(subset(d, step == 0))
    }, 25)
})



