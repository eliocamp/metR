library(metR)
library(ggplot2)
library(data.table)
library(vdiffr)


context("Streamline")
geo <- geopotential[date == date[1]]
geo[, c("u", "v") := GeostrophicWind(gh, lon, lat)]

basic_geom_streamline <- ggplot(geo, aes(lon, lat)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 20)
basic_stat_streamline <- ggplot(geo, aes(lon, lat)) +
    stat_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 20)

test_that("Streamline works", {
    skip_on_ci()
    expect_doppelganger("streamline-base", basic_geom_streamline)
    expect_doppelganger("streamline-base", basic_stat_streamline)
})

test_that("Streamline wraps in x amd y", {
    skip_on_ci()
    expect_doppelganger("streamline-xwrapped",
                        ggplot(geo, aes(lon, lat)) +
                            geom_streamline(aes(dx = u, dy = v), L = 20,
                                            xwrap = c(0, 360)))

    expect_doppelganger("streamline-ywrapped",
                        ggplot(geo, aes(lat, lon)) +
                            geom_streamline(aes(dx = v, dy = u), L = 20,
                                            ywrap = c(0, 360)))
})

test_that("Streamline ignores irregular grids", {
    data <- data.frame(lat = rnorm(10), lon = rnorm(10),
                       u = rnorm(10), v = rnorm(10))
    g <- ggplot(data, aes(lon, lat)) +
        geom_streamline(aes(dx = v, dy = u))

    geo <- rbind(na.omit(geo)[1, u := 1], geo)

    g2 <- ggplot(geo, aes(lon, lat)) +
        geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 20)

    expect_warning(print(g))
    expect_warning(print(g2))

    skip_on_ci()
    suppressWarnings(expect_doppelganger("streamline-irregular", g))
    suppressWarnings(expect_doppelganger("streamline-irregular", g2))
})


