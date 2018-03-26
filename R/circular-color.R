# library(ggplot2)
#
# df <- data.frame(x = rnorm(100), y = rnorm(100))
#
# df$dir <- with(df, atan2(y, x))
# df$t <- 1:nrow(df)
#
# ## The default is, of course, ridiculous.
# (g <- ggplot(df, aes(dir, t)) +
#         geom_point(aes(color = dir)) +
#         coord_polar())
#
# ## First approach. It doesn't work.
# g + scale_color_gradient2(low = "blue", high = "blue", mid = "red")
#
# library(colorspace)
#
# # https://mycarta.wordpress.com/2014/10/30/new-matlab-isoluminant-colormap-for-azimuth-data/
# angle <- seq(0, 2*pi, length.out = 20)
# L <- approx(c(0, 0.5, 1), c(100, 0, 100), xout = seq(0, 1, length.out = 20))$y
#
# plot(circular <- polarLAB(70, 80, angle*180/pi))
# plot(circular2 <- polarLAB(L, 0, 0))
#
# g + scale_color_gradientn(colours = hex(circular, fixup = TRUE))
# g + scale_color_gradientn(colours = hex(circular2, fixup = TRUE))
#
#
# colorblindr::cvd_grid(g + scale_color_gradientn(colours = hex(circular, fixup = TRUE)))
# colorblindr::cvd_grid(g + scale_color_gradientn(colours = hex(circular2, fixup = TRUE)))
#
# ## 'Real' example
# library(data.table)
# library(metR)
# v <- as.data.table(melt(volcano))
# v[, c("dx", "dy") := Derivate(value ~ Var1 + Var2, fill = TRUE)]
# v[, angle := atan2(-dy, -dx)]
#
#
# (g <- ggplot(v, aes(Var1, Var2)) +
#         geom_point(aes(color = value, size = 1.5, alpha = 0.5)) +
#         geom_tile(aes(fill = angle), alpha = 0.5) )
#
# g + scale_fill_gradientn(colours = hex(circular, fixup = TRUE))
# g + scale_fill_gradientn(colours = hex(circular2, fixup = TRUE)) +
#     scale_color_viridis_c()
#
# colorblindr::cvd_grid(g + scale_fill_gradientn(colours = hex(circular, fixup = TRUE)))
# colorblindr::cvd_grid(g + scale_fill_gradientn(colours = hex(circular2, fixup = TRUE)))
#
# L <- approx(c(0, 0.5, 1), c(100, 0, 100), xout = seq(0, 1, length.out = 20))$y
#
# plot(circular <- polarLAB(70, 80, angle*180/pi))
# plot(circular2 <- polarLAB(50*(cos(angle + pi/3) +1 ), 0, 0))
#
#
# v1 <- v
# (g.shade <- ggplot(v1, aes(Var1, Var2)) +
#         geom_tile(aes(fill = angle), alpha = 0.5) +
#     scale_fill_gradientn(colours = hex(circular2, fixup = TRUE)))
#
# grob.shade <- ggplotGrob(g.shade)
#
# (g <- ggplot(v1, aes(Var1, Var2)) +
#     geom_tile(aes(fill = value), alpha = 1) +
#     scale_fill_viridis_c())
#
# g + annotation_custom(grob = grob.shade$grobs[[6]]$children[[3]])
#
#
