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
#' set.seed(42)
#' field$u <- rnorm(nrow(field))
#' field$v <- rnorm(nrow(field))
#' field$V <- with(field, sqrt(u^2 + v^2))
#' field$dir <- with(field, atan2(v, u))*180/pi
#' library(ggplot2)
#' ggplot(field, aes(x, y)) +
#'     geom_arrow(aes(mag = V, angle = dir))
#'
#' @export
#' @family ggplot2 helpers
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = "arrow",
                       position = "identity", ...,
                       start = 0,
                       direction = 1,
                       # scale = 1,
                       min.mag = 0,
                       skip = 0,
                       skip.x = skip,
                       skip.y = skip,
                       arrow.angle = 15,
                       arrow.length = 0.5,
                       arrow.ends = "last",
                       arrow.type = "closed",
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
              # scale = scale,
              skip.x = skip.x,
              skip.y = skip.y,
              min.mag = min.mag,
              ...)
    )
}

GeomArrow <- ggplot2::ggproto("GeomArrow", Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(color = "black", size = 0.5, min.mag = 0,
                             linetype = 1, alpha = NA),
  draw_key = ggplot2::draw_key_path,
  draw_panel = function(data, panel_scales, coord,
                        arrow = arrow, lineend = lineend,
                        start = start, direction = direction,
                        preserve.dir = TRUE) {
      coords <- coord$transform(data, panel_scales)
      unit.delta <- "snpc"
      if (preserve.dir == FALSE) {
          coords$angle <- with(coords, atan2(yend - y, xend - x)*180/pi)
          unit.delta <- "npc"
      }

      coords$dx <- with(coords, mag*cos(angle*pi/180))
      coords$dy <- with(coords, mag*sin(angle*pi/180))

      # from https://stackoverflow.com/questions/47814998/how-to-make-segments-that-preserve-angles-in-different-aspect-ratios-in-ggplot2
      xx <- grid::unit.c(grid::unit(coords$x, "npc"),
                         grid::unit(coords$x, "npc") + grid::unit(coords$dx, unit.delta))
      yy <- grid::unit.c(grid::unit(coords$y, "npc"),
                         grid::unit(coords$y, "npc") + grid::unit(coords$dy, unit.delta))


      mag <- with(coords, mag/max(mag, na.rm = T))
      arrow$length <- unit(as.numeric(arrow$length)*mag, attr(arrow$length, "unit"))

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


StatArrow <- ggplot2::ggproto("StatArrow", ggplot2::Stat,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(min.mag = 0, dx = NULL, dy = NULL,
                               mag = NULL, angle = NULL),
    compute_group = function(data, scales,
                             skip.x = skip.x, skip.y = skip.y,
                             min.mag = min.mag) {
        min.mag <- data$min.mag %||% min.mag

        if (is.null(data$mag) | is.null(data$angle)) {
            if (is.null(data$dx) | is.null(data$dy)) stop("stat_arrow need dx, dy or mag angle (improve mesage!!)")
            data$mag <- with(data, Mag(dx, dy))
            data$angle <- with(data, atan2(dy, dx)*180/pi)
        } else {
            data$dx <- with(data, mag*cos(angle*pi/180))
            data$dy <- with(data, mag*sin(angle*pi/180))
        }

        data <- subset(data, x %in% JumpBy(unique(x), skip.x + 1) &
                             y %in% JumpBy(unique(y), skip.y + 1) &
                             mag >= min.mag)

        data$xend = with(data, x + dx)
        data$yend = with(data, y + dy)
        data

    }
)
