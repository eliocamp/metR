library(ggplot2)
library(vdiffr)

skip_on_ci()
context("text_contour")
data(geopotential)
geo <- subset(geopotential, date == date[1])

test_that("contour_text wokrs", {

    expect_doppelganger("text_contour base",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh))
    )

    expect_doppelganger("text_contour norotate",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh), rotate = FALSE)
    )



    expect_doppelganger("placement_fraction",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh),
                                              label.placer = label_placer_fraction(c(0.25, 0.75)))
    )

    expect_doppelganger("placement_minmax-vertical",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh),
                                              label.placer = label_placer_minmax())
    )

    expect_doppelganger("placement_minmax-horizontal",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh),
                                              label.placer = label_placer_minmax("horizontal"))
    )


    expect_doppelganger("placement_n",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh),
                                              label.placer = label_placer_n(2))
    )


    expect_doppelganger("minsize",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh), skip = 0, min.size = 30)
    )


})


test_that("stroke.colour is aesthetic", {

    expect_doppelganger("stroke-aes",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh, stroke.colour = after_stat(level)),
                                              stroke = 0.2)
                        )
})


test_that("geom_label_contour also work", {

    expect_doppelganger("labels",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh))+
                            geom_label_contour(aes(z = gh, fill = after_stat(level)),
                                               label.r = unit(0.25, "lines"),
                                               label.padding = unit(0.15, "lines"),
                                               label.size = 0)
    )
    expect_doppelganger("labels+text",
                        ggplot(geo, aes(lon, lat)) +
                            geom_contour(aes(z = gh)) +
                            geom_label_contour(aes(z = gh)) +
                            geom_text_contour(aes(z = gh), color = "red", rotate = FALSE)
    )


})
