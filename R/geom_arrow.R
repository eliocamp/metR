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

      if (preserve.dir == FALSE) {
          if (!coord$is_linear()) {

              # full.width <- grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
              # full.height <- grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
              # s <- mean(full.width, full.height)
              #
              # data$mag <- data$mag/s
              # data$dx <- with(data, mag*cos(angle*pi/180))
              # data$dy <- with(data, mag*sin(angle*pi/180))
              # data$xend <- with(data, x + dx)
              # data$yend <- with(data, y + dy)
              #
              stop("geom_arrow does not work with non-linear coordinates if preserve.dir = FALSE. Use geom_vector instead", call. = FALSE)
              # data$group <- 1:nrow(data)
              #
              #
              # starts <- subset(data, select = c(-xend, -yend))
              # ends <- plyr::rename(subset(data, select = c(-x, -y)), c("xend" = "x", "yend" = "y"),
              #                      warn_missing = FALSE)
              #
              # pieces <- rbind(starts, ends)
              # pieces <- pieces[order(pieces$group),]
              # mag <- with(data, mag/max(mag, na.rm = T))
              #
              # arrow$length <- unit(as.numeric(arrow$length)*mag, attr(arrow$length, "unit"))
              #
              # return(GeomPath$draw_panel(pieces, panel_scales, coord, arrow = arrow,
              #                     lineend = lineend))
          } else {
              coords <- coord$transform(data, panel_scales)
              # d <<- data
              # coords <<- coords
              # coord <<- coord
              # panel_scales <<- panel_scales
              aspect <- coord$aspect(panel_scales)
              if (is.null(aspect)) {
                  message("geom_arrow with preserve.dir = FALSE works best with a fixed aspect ratio")
                  delta.unit <- "npc"
                  coords$angle <- with(coords, atan2(yend - y, xend - x)*180/pi)
                  # Not really correct. convertWidth uses the dimensions of the
                  # viewport, not of the plot area :(
                  full.width <- grid::convertWidth(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
                  full.height <- grid::convertHeight(grid::unit(1, "npc"), "cm", valueOnly = TRUE)
                  s <- mean(full.width, full.height)
                  coords$mag <- coords$mag/s
                  coords$dx <- with(coords, mag*cos(angle*pi/180))
                  coords$dy <- with(coords, mag*sin(angle*pi/180))
              } else {
                  delta.unit <- "cm"
                  coords$angle <- with(coords, atan2(dy*aspect, dx)*180/pi)
                  coords$dx <- with(coords, mag*cos(angle*pi/180))
                  coords$dy <- with(coords, mag*sin(angle*pi/180))
              }
          }
      } else {
          coords <- coord$transform(data, panel_scales)
          delta.unit <- "cm"
          coords$dx <- with(coords, mag*cos(angle*pi/180))
          coords$dy <- with(coords, mag*sin(angle*pi/180))
      }

      # from https://stackoverflow.com/questions/47814998/how-to-make-segments-that-preserve-angles-in-different-aspect-ratios-in-ggplot2
      xx <- grid::unit.c(grid::unit(coords$x, "npc") - grid::unit(coords$dx*pivot, delta.unit),
                         grid::unit(coords$x, "npc") + grid::unit(coords$dx*(1 - pivot), delta.unit))
      yy <- grid::unit.c(grid::unit(coords$y, "npc") - grid::unit(coords$dy*pivot, delta.unit),
                         grid::unit(coords$y, "npc") + grid::unit(coords$dy*(1 - pivot), delta.unit))


      mag <- with(coords, mag/max(mag, na.rm = TRUE))
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

        # Pass x, y, xend, yend with different name so they're not
        # scaled and geom has access to them.
        data$xend <- with(data, x + dx)
        data$yend <- with(data, y + dy)
        data$xend.real <- with(data, x + dx)
        data$yend.real <- with(data, y + dy)
        data$x.real <- with(data, x)
        data$y.real <- with(data, y)
        data

    }
)
