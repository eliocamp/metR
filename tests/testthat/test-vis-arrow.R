skip_on_ci()
skip_if_not_installed("vdiffr")

library(vdiffr)
library(ggplot2)

context("arrows")

test_that("arrow guide works", {

    g <- ggplot(seals, aes(long, lat)) +
        geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)
    expect_doppelganger("no-scale",
                        g)
    expect_doppelganger("with-scale",
                        g + scale_mag())
    expect_doppelganger("guide-default", g + scale_mag("Seals velocity"))
    expect_doppelganger("guide-name-guide",
                        g + scale_mag(guide = guide_legend(title = "Velocity (m/s)")))


     g <- ggplot(seals, aes(long, lat)) +
         geom_arrow(aes(dx = delta_long, dy = delta_lat), skip = 1, color = "red") +
         geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 1) +
         scale_mag()

     expect_doppelganger("vector-arrow-example", g)

})

test_that("mag and angle equal dx and dy", {
    dxdy <- ggplot(seals, aes(long, lat)) +
        geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2) +
        scale_mag("Mag(delta_long, delta_lat)")

    magangle <- ggplot(seals, aes(long, lat)) +
        geom_vector(aes(mag = Mag(delta_long, delta_lat),
                        angle = Angle(delta_long, delta_lat)), skip = 2)
    expect_doppelganger("mag and angle", magangle)
    expect_doppelganger("dx and dy", dxdy)
})
