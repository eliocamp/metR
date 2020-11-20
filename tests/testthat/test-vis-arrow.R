library(vdiffr)
library(ggplot2)

context("arrows")
skip_on_ci()
test_that("arrow guide works", {

    g <- ggplot(seals, aes(long, lat)) +
        geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)

    expect_doppelganger("guide-default", g + scale_mag("Seals velocity"))
    expect_doppelganger("guide-name-guide",
                        g + scale_mag(guide = guide_legend(title = "Velocity (m/s)")))
    expect_doppelganger("guide-max-1", g + scale_mag("Seals velocity", max = 1))
    expect_doppelganger("guide-max-2", g + scale_mag("Seals velocity", max = 2))
    expect_doppelganger("guide-max-size-2", g + scale_mag("Seals velocity", max_size = 2))
    expect_doppelganger("guide-mm",  g + scale_mag("Seals velocity", default_unit = "mm"))



     g <- ggplot(seals, aes(long, lat)) +
         geom_arrow(aes(dx = delta_long, dy = delta_lat), skip = 1, color = "red") +
         geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 1) +
         scale_mag()

     expect_doppelganger("vector-arrow-example", g)

})

