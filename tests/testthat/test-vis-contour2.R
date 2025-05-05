skip_on_ci()
skip_if_not_installed("vdiffr")

library(vdiffr)
library(ggplot2)

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


file <- system.file("extdata", "met.txt", package = "metR")
df <- read.table(file, header = FALSE)

test_that("global.breaks work", {

    expect_doppelganger("contour2-global.breaks",
                        ggplot(df, aes(x=V1, y=V2, z=V3)) +
                            geom_contour2(color = "red") +
                            geom_contour2(global.breaks = FALSE) +
                            guides(fill = "none") +
                            facet_grid(. ~ V4))

    # expect_doppelganger("contour2-global.breaks_full",
    #                     ggplot(df, aes(x=V1, y=V2, z=V3)) +
    #                         geom_contour_fill(global.breaks = FALSE) +
    #                         geom_contour2(global.breaks = FALSE) +
    #                         geom_text_contour(global.breaks = FALSE) +
    #                         guides(fill = "none") +
    #                         facet_grid(. ~ V4)
    #                     )
})
test_that("labels work", {
    expect_doppelganger("contour2-labels",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour2(aes(z = gh, label = after_stat(level)))
    )
    expect_doppelganger("aesthetics in contour labels",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour2(aes(z = gh,
                                              label = after_stat(level),
                                              label_colour = after_stat(level),
                                              label_alpha = after_stat(level),
                                              label_size = after_stat(level))) +
                            guides(colour = guide_colorbar(order = 2),
                                   size = guide_legend(order = 1))

    )

})

test_that("contour_tanaka works", {
    expect_doppelganger("tanaka",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour_filled(aes(z = gh)) +
                            geom_contour_tanaka(aes(z = gh))
    )
    # expect_doppelganger("tanaka_smooth",
    #                     ggplot(geo, aes(lon, lat)) +
    #                         geom_contour_filled(aes(z = gh)) +
    #                         geom_contour_tanaka(aes(z = gh), smooth = TRUE)
    # )
})
