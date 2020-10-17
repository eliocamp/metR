library(vdiffr)
library(ggplot2)

skip_on_travis()

context("stat_contour2")
data(geopotential)
geo <- subset(geopotential, date == date[1])
test_that("geom_contour2 mimics geom_contour", {
    expect_doppelganger("contour2-base_geom",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh), color = "red") +
                            geom_contour2(aes(z = gh)) +
                            guides(fill = "none"))

    expect_doppelganger("contour2-base_stat",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh), color = "red") +
                            stat_contour2(aes(z = gh)) +
                            guides(fill = "none"))
})

    test_that("accepts function in breaks", {
        expect_doppelganger("contour2-fun",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour2(aes(z = gh), breaks = MakeBreaks(50)))
})


test_that("global.breaks work", {
    df <- read.table('met.txt', header = FALSE)

    expect_doppelganger("contour2-global.breaks",
                        ggplot(df, aes(x=V1, y=V2, z=V3)) +
                            geom_contour2(color = "red") +
                            geom_contour2(global.breaks = FALSE) +
                            guides(fill = "none") +
                            facet_grid(. ~ V4))

    expect_doppelganger("contour2-global.breaks_full",
                        ggplot(df, aes(x=V1, y=V2, z=V3)) +
                            geom_contour_fill(global.breaks = FALSE) +
                            geom_contour2(global.breaks = FALSE) +
                            geom_text_contour(global.breaks = FALSE) +
                            guides(fill = "none") +
                            facet_grid(. ~ V4))
})


