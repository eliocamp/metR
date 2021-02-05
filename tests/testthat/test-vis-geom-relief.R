library(vdiffr)
library(ggplot2)

skip_on_ci()
data(volcano)
v <- reshape2::melt(volcano)
context("geom_relief")
test_that("geom-relief", {
    expect_doppelganger("volcano-relief",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_relief(aes(z = value))
    )

    # expect_doppelganger("volcano-relief-noraster",
    #                     ggplot(v, aes(Var1, Var2)) +
    #                         geom_relief(aes(z = value), raster = FALSE)
    # )

    expect_doppelganger("volcano-relief-sun.angle",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_relief(aes(z = value), sun.angle = 15)
    )


    expect_doppelganger("volcano-relief-no-interpolate",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_relief(aes(z = value), interpolate = FALSE)
    )


    expect_doppelganger("volcano-relief-shadow",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_relief(aes(z = value), shadow = TRUE)
    )

    expect_doppelganger("volcano-shadow2",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_shadow(aes(z = value))
    )

    expect_doppelganger("volcano-shadow-range",
                        ggplot(v, aes(Var1, Var2)) +
                            geom_shadow(aes(z = value), range = c(0.1, .5))
    )


})
