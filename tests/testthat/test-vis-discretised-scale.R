library(ggplot2)
library(vdiffr)
context("discretised_scale")
skip_on_ci()

v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v <- v + geom_contour_filled()
# scale_fill_ordinal(guide = guide_colorsteps(FALSE))
# discrete_scale("fill", "scale", palette = colorRampPalette(c("#132B43", "#56B1F7")))
test_that("discretized scale works", {


    expect_doppelganger("discretised-default", v + scale_fill_discretised())

    expect_doppelganger("discretised-change-limits", v + scale_fill_discretised(limits = c(0, 0.06)))
    expect_doppelganger("discretised-change-limits2", v + scale_fill_discretised(limits = c(0, 0.025)))

    expect_doppelganger("discretised-divergent",
                        v + scale_fill_divergent_discretised(midpoint = 0.02))

    expect_error(print(ggplot(faithfuld, aes(waiting, eruptions, z = density)) +
                           geom_contour_filled(aes(fill = stat(level_mid))) +
                           scale_fill_discretised()), "Discretised scales only support discrete data")

    expect_doppelganger("discretised-retrofitted",
                        v + scale_fill_distiller(super = ScaleDiscretised,
                                                 guide = guide_colorsteps()))
    expect_doppelganger("user-supplied breaks",
                        v + scale_fill_discretised(breaks = c(0, 0.015, 0.02, 0.045))
    )

})
