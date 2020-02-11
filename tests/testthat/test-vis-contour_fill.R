library(vdiffr)
library(data.table)

context("contour_fill")

test_that("geom_contour_fill works", {
    skip_on_ci()
    expect_doppelganger("contour_fill-base",
                        ggplot(geopotential[date == date[1]], aes(lon, lat)) +
                            geom_contour_fill(aes(z = gh)) +
                            guides(fill = "none"))

    expect_doppelganger("contour_fill-base-stat",
                        ggplot(geopotential[date == date[1]], aes(lon, lat)) +
                            stat_contour_fill(aes(z = gh)) +
                            guides(fill = "none"))
})

test_that("interpolation", {
    data <- geopotential[date == date[1]][lat %between% c(-60, -30) & lon %between% c(90, 120), gh := NA]
    g <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh)) +
        guides(fill = "none")

    expect_warning(print(g), "data must not have missing values. Use na.fill = TRUE or impute them before plotting.")
    skip_on_ci()
    suppressWarnings(expect_doppelganger("contour_fill-nofill", g))

    g1 <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh), na.fill = TRUE) +
        guides(fill = "none")
    expect_warning(print(g1), "imputing missing values")
    suppressWarnings(expect_doppelganger("contour_fill-fill", g1))

    data <- geopotential[date == date[1]][!(lat %between% c(-60, -30) & lon %between% c(90, 120))]

    g2 <- ggplot(data, aes(lon, lat)) +
        geom_contour_fill(aes(z = gh), complete = FALSE) +
        guides(fill = "none")
    expect_warning(print(g2), "data must be a complete regular grid")

    suppressWarnings(expect_doppelganger("contour_fill-nofill", g2))
})



