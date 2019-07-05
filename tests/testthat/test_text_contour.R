
context("tex_contour")
v <- reshape2::melt(volcano)
g <- ggplot(v, aes(Var1, Var2, z = value)) +
    geom_contour()

# test_that("default text works", {
#     expect_doppelganger("text_contour-default", g + geom_text_contour())
#     expect_doppelganger("text_contour-norotate", g + geom_text_contour(rotate = FALSE))
#     expect_doppelganger("text_contour-skip", g + geom_text_contour(skip = 0))
# })
#
# test_that("shadows work", {
#     expect_doppelganger("text_contour-stroke",
#                         g + geom_text_contour(stroke = 0.2))
#     expect_doppelganger("text_contour-stroke.color",
#                         g + geom_text_contour(stroke = 0.2, stroke.color = "red"))
# })

