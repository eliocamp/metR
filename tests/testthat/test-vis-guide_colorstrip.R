library(metR)
library(vdiffr)
library(ggplot2)

skip_on_ci()

context("guide_colourstrip")
data(geopotential)
geo <- geopotential[date == date[1]]
base <- ggplot(geo, aes(lon, lat)) +
    geom_raster(aes(fill = gh))

test_that("guide works", {
    expect_doppelganger("default-guide_colourstrip",
                        base + guides(fill = guide_colourstrip()))
    expect_doppelganger("inside-guide_colorstip",
                        base + guides(fill = guide_colourstrip(inside = TRUE)))
    expect_doppelganger("reverse-guide_colorstip",
                        base + guides(fill = guide_colourstrip(reverse = TRUE)))
    expect_doppelganger("horizontal-direction",
                        base + guides(fill = guide_colourstrip(direction = "horizontal", barwidth = 10)) +
                            theme(legend.position = "bottom"))

    expect_doppelganger("colorstrip_inside",
        base + guides(fill = guide_colourstrip(inside = TRUE))
    )
})

