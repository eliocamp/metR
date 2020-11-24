library(ggplot2)
library(data.table)
library(vdiffr)

skip_on_ci()

context("Streamline")
geo <- geopotential[date == date[1]]
geo[, c("u", "v") := GeostrophicWind(gh, lon, lat)]

basic_geom_streamline <- ggplot(geo, aes(lon, lat)) +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), res = 1/3, L = 10, skip = 2)

basic_stat_streamline <- ggplot(geo, aes(lon, lat)) +
    stat_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), res = 1/3, L = 10, skip = 2)

test_that("Streamline works", {
    # skip_on_ci()
    writer_warnings <- function(...) suppressWarnings(vdiffr::write_svg(...))

    expect_warning(invisible(print(basic_geom_streamline)), "performing only 1 integration step")
    expect_doppelganger("streamline-base", basic_geom_streamline, writer = writer_warnings)
    expect_doppelganger("streamline-base", basic_stat_streamline, writer = writer_warnings)
})

test_that("Streamline wraps in x amd y", {
    # skip_on_ci()
    expect_doppelganger("streamline-xwrapped",
                        ggplot(geo, aes(lon, lat)) +
                            geom_streamline(aes(dx = u, dy = v), L = 20,
                                            skip = 2, res = 1/2,
                                            xwrap = c(0, 360)))

    expect_doppelganger("streamline-ywrapped",
                        ggplot(geo, aes(lat, lon)) +
                            geom_streamline(aes(dx = v, dy = u), L = 20,
                                            skip = 2, res = 1/2,
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

    expect_warning(invisible(print(g)), "x and y do not define a regular grid")
    expect_warning(invisible(print(g2)), "x and y do not define a regular grid")

    skip_on_ci()
})


