#' Arrows
#'
#' Unlike [geom_vector], `geom_arrow()` preserves direction in coordinate
#' transformation and different aspect ratios.
#'
#' @inheritParams geom_vector
#' @param direction Direction of rotation in degrees.
#' @param start Starting angle for rotation in degrees.
#'
#' @details
#' Direction and start allows to work with different standards. For the
#' meteorological standard, for example, use `star = -90` and `direction = -1`.
#'
#' @section Aesthetics:
#' `geom_vector` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **mag**
#' \item **angle**
#' \item \code{alpha}
#' \item \code{colour}
#' \item \code{linetype}
#' \item \code{size}
#' \item \code{lineend}
#' }
#'
#' @examples
#' field <- expand.grid(x = seq.Date(as.Date("2017-01-01"), as.Date("2017-01-31"), "2 days"),
#'                      y = 1:10)
#' field$u <- rnorm(nrow(field))
#' field$v <- rnorm(nrow(field))
#' field$V <- with(field, sqrt(u^2 + v^2))
#' field$dir <- with(field, atan2(v, u))*180/pi
#' library(ggplot2)
#' ggplot(field, aes(x, y)) +
#'     geom_arrow(aes(mag = V, angle = dir), color = "red",
#'                scale = 0.05, start = 0)
#'
#' @export
#' @family ggplot2 helpers
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = "identity",
                       position = "identity", ...,
                       start = 0,
                       direction = 1,
                       scale = 1,
                       min.mag = 0,
                       skip = 0,
                       skip.x = skip,
                       skip.y = skip,
                       arrow.angle = 15,
                       arrow.length = 0.2,
                       arrow.ends = "last",
                       arrow.type = "open",
                       arrow = grid::arrow(arrow.angle, unit(arrow.length, "lines"),
                                           ends = arrow.ends, type = arrow.type),
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    layer(geom = GeomArrow,
          mapping = mapping,
          data = data,
          stat = stat,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
              start = start,
              direction = direction,
              arrow = arrow,
              lineend = lineend,
              na.rm = na.rm,
              scale = scale,
              skip.x = skip.x,
              skip.y = skip.y,
              min.mag = min.mag,
              ...)
    )
}
GeomArrow <- ggplot2::ggproto("GeomArrow", Geom,
  required_aes = c("x", "y", "mag", "angle"),
  default_aes = ggplot2::aes(color = "black", scale = 1, size = 0.5, min.mag = 0,
                             linetype = 1, alpha = NA),
  draw_key = draw_key_path,
  draw_panel = function(data, panel_scales, coord, skip.x = skip.x,
                        skip.y = skip.y, arrow = arrow, lineend = lineend,
                        start = start, direction = direction) {
      coords <- coord$transform(data, panel_scales)
      coords <- subset(coords,
                       x %in% JumpBy(unique(x), skip.x + 1) &
                           y %in% JumpBy(unique(y), skip.y + 1) &
                           mag >= min.mag)
      coords$angle <- start + coords$angle*sign(direction)
      coords$dx <- with(coords, mag*cos(angle*pi/180)*scale)
      coords$dy <- with(coords, mag*sin(angle*pi/180)*scale)

      # from https://stackoverflow.com/questions/47814998/how-to-make-segments-that-preserve-angles-in-different-aspect-ratios-in-ggplot2
      xx <- grid::unit.c(grid::unit(coords$x, "npc"),
                         grid::unit(coords$x, "npc") + grid::unit(coords$dx, "snpc"))
      yy <- grid::unit.c(grid::unit(coords$y, "npc"),
                         grid::unit(coords$y, "npc") + grid::unit(coords$dy, "snpc"))

      pol <- grid::polylineGrob(x = xx, y = yy,
                                default.units = "npc",
                                arrow = arrow,
                                gp = grid::gpar(col = coords$colour,
                                                fill = scales::alpha(coords$colour, coords$alpha),
                                                alpha = ifelse(is.na(coords$alpha), 1, coords$alpha),
                                                lwd = coords$size*.pt,
                                                lty = coords$linetype,
                                                lineend = lineend),
                                id = rep(seq(nrow(coords)), 2))
      pol
  })
