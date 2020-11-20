library(vdiffr)
library(ggplot2)
context("guide_vector and scale_mag")
skip_on_ci()

library(ggplot2)
base <- ggplot(seals, aes(long, lat)) +
    geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)



test_that("scale works", {
    expect_doppelganger("no-scale",
                        base)
    expect_doppelganger("with-scale",
                        base + scale_mag())

    expect_doppelganger("name",
                        base + scale_mag("Mag"))


    expect_doppelganger("small-arrow",
                        base + scale_mag(max_size = 0.5))

    expect_doppelganger("big-arrow",
                        base + scale_mag(max_size = 3))

    expect_doppelganger("set-max_arrow",
                        base + scale_mag(max = 5))
})



