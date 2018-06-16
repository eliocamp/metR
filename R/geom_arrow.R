#' Arrows
#'
#' Unlike [geom_vector], `geom_arrow()` preserves direction in coordinate
#' transformation and different aspect ratios.
#'
#' @inheritParams geom_vector
#' @param direction Direction of rotation in degrees.
#' @param start Starting angle for rotation in degrees.
#' @param pivot numeric indicating where to pivot the arrow where '0 means at the
#' begining and 1 meanns at the end.
#' @param preserve.dir logical whether to preserve direction or not
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
#' \item either **mag** and **angle**, or **dx** and **dy**
#' \item \code{alpha}
#' \item \code{colour}
#' \item \code{linetype}
#' \item \code{size}
#' \item \code{lineend}
#' }
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' data(geopotential)
#'
#' geopotential <- copy(geopotential)[date == date[1]]
#' geopotential[, gh.z := Anomaly(gh), by = .(lat)]
#' geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]
#'
#' (g <- ggplot(geopotential, aes(lon, lat)) +
#'     geom_arrow(aes(dx = u, dy = v), skip.x = 3, skip.y = 2,
#'                pivot = 0.5) +
#'     scale_mag())
#'
#' @export
#' @family ggplot2 helpers
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = "arrow",
                       position = "identity",
                       ...,
                       start = 0,
                       direction = 1,
                       pivot = 0.5,
                       preserve.dir = FALSE,
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
              pivot = pivot,
              preserve.dir = preserve.dir,
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


draw_key_vector <- function (data, params, size) {
    data$linetype[is.na(data$linetype)] <- 0
    grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                       gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                       lwd = data$size * .pt,
                                       lty = data$linetype,
                                       lineend = "butt"),
                       arrow = params$arrow)
}

#' @rdname geom_arrow
#' @usage NULL
#' @format NULL
#' @export
GeomArrow <- ggplot2::ggproto("GeomArrow", Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(color = "black", size = 0.5, min.mag = 0,
                             linetype = 1, alpha = NA, mag = 0,
                             angle = 0),
  draw_key =  draw_key_vector,
  draw_panel = function(data, panel_scales, coord,
                        arrow = arrow, lineend = lineend,
                        start = start, direction = direction,
                        preserve.dir = FALSE, pivot = 0.5) {

      mag <- with(data, mag/max(mag, na.rm = TRUE))
      arrow$length <- unit(as.numeric(arrow$length)*mag, attr(arrow$length, "unit"))
      if (preserve.dir == FALSE) {
                    # For non linear coords
          data$group <- seq(nrow(data))
          data$piece <- 1
          data2 <- data
          data2$piece <- 2

          # Approximation for non linear coords.
          data2$x <- with(data, x + dx/10000)
          data2$y <-  with(data, y + dy/10000)

          coords <- coord$transform(data, panel_scales)
          coords2 <- coord$transform(data2, panel_scales)

          coords$xend <- coords2$x
          coords$yend <- coords2$y
          coords$dx <- with(coords, xend - x)/100
          coords$dy <- with(coords, yend - y)/100

          pol <- vectorGrob(x = coords$x, y = coords$y,
                           dx = coords$dx, dy = coords$dy,
                           length = unit(coords$mag, "cm"),
                           pivot = pivot,
                           preserve.dir = preserve.dir,
                           default.units = "npc",
                           arrow = arrow,
                           gp = grid::gpar(col = coords$colour,
                                           fill = scales::alpha(coords$colour, coords$alpha),
                                           alpha = ifelse(is.na(coords$alpha), 1, coords$alpha),
                                           lwd = coords$size*.pt,
                                           lty = coords$linetype,
                                           lineend = lineend))

      } else {
          coords <- coord$transform(data, panel_scales)
          pol <- arrowGrob(x = coords$x, y = coords$y,
                           angle = coords$angle, length = unit(coords$mag, "cm"),
                           pivot = pivot,
                           preserve.dir = preserve.dir,
                           default.units = "native",
                           arrow = arrow,
                           gp = grid::gpar(col = coords$colour,
                                           fill = scales::alpha(coords$colour, coords$alpha),
                                           alpha = ifelse(is.na(coords$alpha), 1, coords$alpha),
                                           lwd = coords$size*.pt,
                                           lty = coords$linetype,
                                           lineend = lineend))
      }
      pol
  })

#' @rdname geom_arrow
#' @usage NULL
#' @format NULL
#' @export
StatArrow <- ggplot2::ggproto("StatArrow", ggplot2::Stat,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(min.mag = 0, dx = NULL, dy = NULL,
                               mag = NULL, angle = NULL),
    compute_group = function(data, scales,
                             skip.x = skip.x, skip.y = skip.y,
                             min.mag = min.mag) {
        min.mag <- data$min.mag %||% min.mag

        if (is.null(data$mag) | is.null(data$angle)) {
            if (is.null(data$dx) | is.null(data$dy)) {
                stop("stat_arrow need dx, dy or mag angle (improve mesage!!)")
            }
            data$mag <- with(data, Mag(dx, dy))
            data$angle <- with(data, atan2(dy, dx)*180/pi)
        } else {
            data$dx <- with(data, mag*cos(angle*pi/180))
            data$dy <- with(data, mag*sin(angle*pi/180))
        }

        data <- subset(data, x %in% JumpBy(unique(x), skip.x + 1) &
                             y %in% JumpBy(unique(y), skip.y + 1) &
                             mag >= min.mag)

        data

    }
)
