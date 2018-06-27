library(metR)
library(vdiffr)

context("stat_contour2")
data(geopotential)
geo <- geopotential[date == date[1]]
test_that("geom_contour2 mimics geom_contour", {
    expect_doppelganger("contour2-base_geom",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour2(aes(z = gh)))
    expect_doppelganger("contour2-base_stat",
                        ggplot(geo, aes(lon, lat)) +
                            stat_contour2(aes(z = gh)))
})

test_that("accepts function in breaks", {
    expect_doppelganger("contour2-fun",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour2(aes(z = gh), breaks = MakeBreaks(50)))
})
