library(ggplot2)

geom_arrow2 <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, scale = 1,...) {
    layer(geom = GeomArrow2,
          mapping = mapping,
          data = data,
          stat = stat,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, scale = scale, ...)
    )
}
GeomArrow2 <- ggplot2::ggproto("GeomArrow2", Geom,
   required_aes = c("x", "y", "mag", "angle"),
   default_aes = ggplot2::aes(color = "black", scale = 1),
   draw_key = draw_key_polygon,
   draw_panel = function(data, panel_scales, coord, scale = 1) {
       coor <<- coord
       coords <- coord$transform(data, panel_scales)
       # rx <- resolution(coords$x, zero = FALSE)
       # ry <- resolution(coords$y, zero = FALSE)
       # mr <- min(rx, ry)
       # m <<- mr
       Mmag <- max(coords$mag)
       coords$mag <- with(coords, mag/Mmag*coords$scale)
       data2 <<- data

       panel_scales2 <<- panel_scales
       # v <- grid::current.viewport()
       # xs <- abs(diff(v$xscale))
       # ys <- abs(diff(v$yscale))

       coords$dx <- with(coords, cos(angle)*mag)*scale
       coords$dy <- with(coords, sin(angle)*mag)*scale

       # coords$x2 <- grid::convertX(grid::unit(coords$x, "npc"), "mm", valueOnly = TRUE)
       # coords$y2 <- grid::convertY(grid::unit(coords$y, "npc"), "mm", valueOnly = TRUE)
       # sx <- diff(range(coords2$x2))/diff(range(coords$x))
       # sy <<- diff(range(y2))
       coords2 <<- coords
       pol <- grid::polylineGrob(x = c(coords$x,
                                        coords$x + coords$dx),
                                  y = c(coords$y,
                                        coords$y + coords$dy),
                                  default.units = "snpc",
                                  arrow = grid::arrow(angle = 15, length = unit(0.5, "lines")),
                                  gp = grid::gpar(col = coords$colour),
                                  id = rep(seq(nrow(coords)), 2))
       pol

   })


field <- expand.grid(x = seq.Date(as.Date("2017-01-01"), as.Date("2017-01-31"), "2 days"),
                     y = 1:10)
field$u <- rnorm(nrow(field))
field$v <- rnorm(nrow(field))
field$V <- with(field, sqrt(u^2 + v^2))
field$dir <- with(field, atan2(v, u))*180/pi

ggplot(field, aes(x, y)) +
    geom_point() +
    geom_arrow2(aes(mag = V, angle = dir), scale = 0.5)


#
#
# df <- as.data.table(expand.grid(x = seq.Date(as.Date("2017-01-01"), as.Date("2017-01-10"), by = "1 day"),
#                                 y = 1:10))
# df[, v := abs(rnorm(.N))]
# df[, dir := sample(0:360,  .N, replace = TRUE)]
#
# ggplot(df, aes(x, y)) +
#     geom_point() +
#     geom_arrow(aes(mag = v, angle = dir)) +
#     geom_arrow2(aes(mag = v, angle = dir*pi/180), color = "red", scale = 0.1)
#     # coord_equal()
#     # coord_polar()

    # coord_equal()
    # coord_polar()m



