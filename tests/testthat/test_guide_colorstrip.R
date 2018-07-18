library(metR)
library(vdiffr)
library(ggplot2)

context("guide_colorstrip")
data(geopotential)
geo <- geopotential[date == date[1]]
base <- ggplot(geo, aes(lon, lat)) +
    geom_raster(aes(fill = gh))

test_that("guide works", {
    expect_doppelganger("default-guide_colorstrip",
                        base + guides(fill = guide_colorstrip()))
    expect_doppelganger("inside-guide_colorstip",
                        base + guides(fill = guide_colorstrip(inside = TRUE)))
    expect_doppelganger("reverse-guide_colorstip",
                        base + guides(fill = guide_colorstrip(reverse = TRUE)))
    expect_doppelganger("horizontal-direction",
                        base + guides(fill = guide_colorstrip(direction = "horizontal", barwidth = 10)) +
                            theme(legend.position = "bottom"))
})

