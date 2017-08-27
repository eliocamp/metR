#' Arrows
#'
#' Parametrization of [ggplot2::geom_segment] by location and displacement that
#' also defaults to drawing arrows at the end of the segment. Useful for plotting
#' vector fields characterized by magnitude of the `x` and `y` components.
#'
#' @param skip numeric vector specifying number of gridpoints not to draw in
#' the x and y direction; recicled if not of length 2
#' @param min.mag minimum magnitude of the plotted vectors
#' @param arrow.size size of the arrow (see [grid::arrow])
#' @param arrow.angle angle of the arrow (see [grid::arrow])
#' @inheritParams ggplot2::geom_segment
#'
#' @examples
#' speed <- expand.grid(lon = 1:360, lat = -90:0)
#' speed <- transform(speed,
#'                    vx = 1,
#'                    vy = 0.3)
#' library(ggplot2)
#' g <- ggplot(speed, aes(lon, lat)) +
#'     geom_arrow(aes(dx = vx, dy = vy), scale = 5,
#'                skip = c(7, 5), arrow.size = 0.2) +
#'     ylim(c(-90, 0))
#' g
#'
# Note that the segments respond to coordinate
#' g + coord_polar()
#'
#' @export
#' @family ggplot2 helpers
#' @import ggplot2
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = StatArrow,
                       position = "identity",
                       ...,
                       arrow.size = 0.5,
                       arrow.angle = 14,
                       lineend = "butt",
                       skip = 0,
                       min.mag = 0,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSegment,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow(arrow.angle, unit(arrow.size, "lines")),
            lineend = lineend,
            na.rm = na.rm,
            skip = skip,
            min.mag = min.mag,
            ...
        )
    )
}

#' @import ggplot2
StatArrow <- ggplot2::ggproto("StatArrow", ggplot2::Stat,
                     required_aes = c("x", "y", "dx", "dy"),
                     default_aes = ggplot2::aes(scale = 0.5),

                     compute_group = function(data, scales, scale = 0.4, skip = skip,
                                              min.mag = min.mag) {
                         if (length(skip) < 2) {
                             skip[2] <- skip[1]
                         }
                         x.skip <- skip[1]
                         y.skip <- skip[2]

                         data <- subset(data, x %in% JumpBy(unique(data$x), x.skip + 1) &
                                            y %in% JumpBy(unique(data$y), y.skip + 1) &
                                            sqrt(dx^2 + dy^2) >= min.mag)
                         transform(data,
                                   xend = x + dx*abs(scale),
                                   yend = y + dy*abs(scale))
                     }

)

# stat_arrow <- function(mapping = NULL, data = NULL,
#                        geom = "segment", position = "identity",
#                        ...,
#                        na.rm = FALSE,
#                        show.legend = NA,
#                        arrow.size = 0.2,
#                        arrow.angle = 14,
#                        inherit.aes = TRUE) {
#     layer(
#         data = data,
#         mapping = mapping,
#         stat = StatArrow,
#         geom = geom,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(
#             na.rm = na.rm,
#             arrow = arrow(arrow.angle, unit(arrow.size, "lines")),
#             ...
#         )
#     )
# }
#
# GeomArrow <- function(arrow.angle = 14, arrow.size = 0.2){
#     ggproto("GeomArrow", GeomSegment,
#             arrow = arrow(arrow.angle, unit(arrow.size, "lines")))
# }
