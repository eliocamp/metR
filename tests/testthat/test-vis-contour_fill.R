library(vdiffr)
library(data.table)

context("contour_fill")

test_that("geom_contour_fill works", {
    expect_doppelganger("contour_fill-base",
                        ggplot(geopotential[date == date[1]], aes(lon, lat)) +
                            geom_contour_fill(aes(z = gh)))

    expect_doppelganger("contour_fill-base-stat",
                        ggplot(geopotential[date == date[1]], aes(lon, lat)) +
                            stat_contour_fill(aes(z = gh)))
})

test_that("interpolation", {
    data <- geopotential[date == date[1]][lat %between% c(-60, -30) & lon %between% c(90, 120), gh := NA]
    g <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh))

    expect_warning(print(g), "data must not have missing values. Use na.fill = TRUE or impute them before plotting.")

    suppressWarnings(expect_doppelganger("contour_fill-nofill", g))

    g <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh), na.fill = TRUE)
    expect_warning(print(g), "imputing missing values")
    suppressWarnings(expect_doppelganger("contour_fill-fill", g))

    data <- geopotential[date == date[1]][!(lat %between% c(-60, -30) & lon %between% c(90, 120))]

    g <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh), complete = FALSE)
    expect_warning(print(g), "data must be a complete regular grid")

    suppressWarnings(expect_doppelganger("contour_fill-nofill", g))
})




