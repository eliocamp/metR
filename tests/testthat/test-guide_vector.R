# library(metR)
#
# context("guide_vector and scale_mag")
#
# data <- expand.grid(x = 1:10, y = 1:10)
# data$u <- rnorm(100)
# data$v <- rnorm(100)
#
# base <- ggplot(data, aes(x,  y))+
#     geom_arrow(aes(dx = u, dy = v))
#
# test_that("scale works", {
#     expect_doppelganger("default-guide_vector",
#                         base + scale_mag())
#     expect_doppelganger("small-arrow",
#                         base + scale_mag(max_size = 0.5))
#     expect_doppelganger("set-max_arrow",
#                         base + scale_mag(max = 5))
# })
#
# test_that("guide works", {
#     expect_doppelganger("change-units",
#                         base + scale_mag(guide = guide_vector(default.unit = "mm")))
# })
#
# base + scale_mag(labels = "10 m/s", max = 10)
