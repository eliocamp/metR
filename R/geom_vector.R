#' Vectors
#'
#' Parametrization of [ggplot2::geom_segment] by location and displacement that
#' also defaults to drawing arrows at the end of the segment. Useful for plotting
#' vector fields characterized by magnitude of the `x` and `y` components.
#'
#' @param scale,scale.x,scale.y multiplicative scales for adjusting the size of the vectors.
#' @param min.mag minimum magnitude of the plotted vectors
#' @param skip,skip.x,skip.y numeric specifying number of gridpoints not to draw in the x and y direction.
#' @param arrow.length,arrow.angle,arrow.ends,arrow.type parameters passed to [grid::arrow]
#' @inheritParams ggplot2::geom_segment
#'
#' @examples
#' speed <- expand.grid(lon = 1:360, lat = -90:0)
#' speed <- transform(speed,
#'                    vx = 1,
#'                    vy = 0.3)
#' library(ggplot2)
#' (g <- ggplot(speed, aes(lon, lat)) +
#'     geom_vector(aes(dx = vx, dy = vy), scale = 5,
#'     skip = 7, skip.x = 8))
#'
#' # Every parameter (except skip) can be mapped to data.
#' # this can be usefull for masking vectors with a variable
#' # min.mag.
#'  ggplot(speed, aes(lon, lat)) +
#'     geom_vector(aes(dx = vx, dy = vy, scale.x = 5*cos(lon*pi/180)),
#'     skip = 7)
#'
#' # Note that the segments respond to coordinate
#' g + coord_polar()
#'
#' # Sverdrup transport
#' library(data.table)
#' b <- 10
#' d <- 10
#' grid <- as.data.table(expand.grid(x = seq(1, d, by = 0.5),
#'                                   y = seq(1, b, by = 0.5)))
#' grid[, My := -sin(pi*y/b)*pi/b]
#' grid[, Mx := -pi^2/b^2*cos(pi*y/b)*(d - x)]
#'
#' ggplot(grid, aes(x, y)) +
#'     geom_vector(aes(dx = Mx, dy = My))
#'
#' @export
#' @family ggplot2 helpers
#' @import ggplot2
geom_vector <- function(mapping = NULL, data = NULL,
                       stat = StatVector,
                       position = "identity",
                       ...,
                       scale = 1,
                       scale.x = scale,
                       scale.y = scale,
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
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSegment,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            skip.x = skip.x,
            skip.y = skip.y,
            min.mag = min.mag,
            scale.x = scale.x,
            scale.y = scale.y,
            ...
        )
    )
}

#' @import ggplot2
StatVector <- ggplot2::ggproto("StatVector", ggplot2::Stat,
    required_aes = c("x", "y", "dx", "dy"),
    default_aes = ggplot2::aes(scale = 1, scale.x = scale, scale.y = scale, min.mag = 0),

    compute_group = function(data, scales, scale.x = scale.x, scale.y = scale.y,
                             skip.x = skip.x, skip.y = skip.y,
                             min.mag = min.mag) {
        min.mag <- data$min.mag %||% min.mag

        scale.x <- data$scale.x %||% data$scale %||% scale.x
        scale.y <- data$scale.y %||% data$scale %||% scale.y

        data$xend = with(data, x + dx*abs(scale.x))
        data$yend = with(data, y + dy*abs(scale.y))

        data <- subset(data, x %in% JumpBy(unique(x), skip.x + 1) &
                           y %in% JumpBy(unique(y), skip.y + 1) &
                           sqrt(dx^2 + dy^2) >= min.mag)
        data
        }
)
