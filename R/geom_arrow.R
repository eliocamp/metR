#' Arrows
#'
#' Parametrization of [ggplot2::geom_segment] either by location and displacement
#' or by magnitude and angle with default arrows. `geom_arrow()` is the same as
#' `geom_vector()` but defaults to preserving the direction under coordinate
#' transformation and different plot ratios.
#'
#' @param min.mag minimum magnitude for plotting vectors
#' @param skip,skip.x,skip.y numeric specifying number of gridpoints not to draw
#'  in the x and y direction
#' @param arrow.length,arrow.angle,arrow.ends,arrow.type parameters passed to
#' [grid::arrow]
#' @inheritParams ggplot2::geom_segment
#' @param direction direction of rotation (counter-clockwise or clockwise)
#' @param start starting angle for rotation in degrees
#' @param pivot numeric indicating where to pivot the arrow where 0 means at the
#' beginning and 1 means at the end.
#' @param preserve.dir logical indicating whether to preserve direction or not
#'
#' @details
#' Direction and start allows to work with different standards. For the
#' meteorological standard, for example, use `star = -90` and `direction = "cw"`.
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
#'
#' data(seals)
#' # If the velocity components are in the same units as the axis,
#' # geom_vector() (or geom_arrow(preserve.dir = TRUE)) might be a better option
#' ggplot(seals, aes(long, lat)) +
#'     geom_arrow(aes(dx = delta_long, dy = delta_lat), skip = 1, color = "red") +
#'     geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 1) +
#'     scale_mag()
#'
#' data(geopotential)
#' geopotential <- copy(geopotential)[date == date[1]]
#' geopotential[, gh.z := Anomaly(gh), by = .(lat)]
#' geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]
#'
#' (g <- ggplot(geopotential, aes(lon, lat)) +
#'     geom_arrow(aes(dx = dlon(u, lat), dy = dlat(v)), skip.x = 3, skip.y = 2,
#'                color = "red") +
#'     geom_vector(aes(dx = dlon(u, lat), dy = dlat(v)), skip.x = 3, skip.y = 2) +
#'     scale_mag(max_size = 2, guide = "none"))
#'
#' # A dramatic illustration of the difference between arrow and vector
#' g + coord_polar()
#'
#' # When plotting winds in a lat-lon grid, a good way to have both
#' # the correct direction and an interpretable magnitude is to define
#' # the angle by the longitud and latitude displacement and the magnitude
#' # by the wind velocity. That way arrows are always parallel to streamlines
#' # and their magnitude are in the correct units.
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.z)) +
#'     geom_vector(aes(angle = atan2(dlat(v), dlon(u, lat))*180/pi,
#'                    mag = Mag(v, u)), skip = 1, pivot = 0.5) +
#'     scale_mag()
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
#'     geom_arrow(aes(dx = Mx, dy = My))
#'
#' @export
#' @family ggplot2 helpers
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = "arrow",
                       position = "identity",
                       ...,
                       start = 0,
                       direction = c("ccw", "cw"),
                       pivot = 0.5,
                       preserve.dir = TRUE,
                       min.mag = 0,
                       skip = 0,
                       skip.x = skip,
                       skip.y = skip,
                       arrow.angle = 15,
                       arrow.length = 0.5,
                       arrow.ends = "last",
                       arrow.type = "closed",
                       arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"),
                                           ends = arrow.ends, type = arrow.type),
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  if (is.null(mapping$mag) & is.null(mapping$angle)) {
    dx <- paste0(substr(deparse(mapping$dx), 2, nchar(deparse(mapping$dx))))
    dy <- paste0(substr(deparse(mapping$dy), 2, nchar(deparse(mapping$dy))))
    mag <- paste0("sqrt(", dx, "^2 + ", dy, "^2)" )
    angle <- paste0("atan2(", dy, ", ", dx, ")*180/pi")
    mapping2 <- ggplot2::aes_string(mag = mag, angle= angle)
    mapping$dx <- NULL
    mapping$dy <- NULL
    mapping$mag <- mapping2$mag
    mapping$angle <- mapping2$angle

  }


  ggplot2::layer(geom = GeomArrow,
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
                   skip.x = skip.x,
                   skip.y = skip.y,
                   min.mag = min.mag,
                   ...)
  )
}

#' @export
#' @rdname geom_arrow
geom_vector <- function(mapping = NULL, data = NULL,
                        stat = "arrow",
                        position = "identity",
                        ...,
                        start = 0,
                        direction = c("ccw", "cw"),
                        pivot = 0.5,
                        preserve.dir = FALSE,
                        min.mag = 0,
                        skip = 0,
                        skip.x = skip,
                        skip.y = skip,
                        arrow.angle = 15,
                        arrow.length = 0.5,
                        arrow.ends = "last",
                        arrow.type = "closed",
                        arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"),
                                            ends = arrow.ends, type = arrow.type),
                        lineend = "butt",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  if (is.null(mapping$mag) & is.null(mapping$angle)) {
    dx <- paste0(substr(deparse(mapping$dx), 2, nchar(deparse(mapping$dx))))
    dy <- paste0(substr(deparse(mapping$dy), 2, nchar(deparse(mapping$dy))))
    mag <- paste0("sqrt(", dx, "^2 + ", dy, "^2)" )
    angle <- paste0("atan2(", dy, ", ", dx, ")*180/pi")
    mapping2 <- ggplot2::aes_string(mag = mag, angle= angle)
    mapping$dx <- NULL
    mapping$dy <- NULL
    mapping$mag <- mapping2$mag
    mapping$angle <- mapping2$angle

  }


  ggplot2::layer(geom = GeomArrow,
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
                   skip.x = skip.x,
                   skip.y = skip.y,
                   min.mag = min.mag,
                   ...)
  )
}


draw_key_vector <- function (data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0
  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
                     gp = grid::gpar(col = ggplot2::alpha(data$colour, data$alpha),
                                     lwd = data$size * ggplot2::.pt,
                                     lty = data$linetype,
                                     lineend = "butt"),
                     arrow = params$arrow)
}

#' @rdname geom_arrow
#' @usage NULL
#' @format NULL
#' @export
GeomArrow <- ggplot2::ggproto("GeomArrow", ggplot2::Geom,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(color = "black", size = 0.5, min.mag = 0,
                             linetype = 1, alpha = NA,
                             angle = 0, mag = 0),
  draw_key =  draw_key_vector,
  draw_panel = function(data, panel_scales, coord,
                        arrow = arrow, lineend = lineend,
                        start = start, direction = direction,
                        preserve.dir = FALSE, pivot = 0.5) {
    if (!is.finite(pivot)) {
      stop("pivot must be a number between 0 and 1", call. = FALSE)
    }
    if (pivot > 1) {
      pivot <- 1
      warning("pivot greater than 1, setting it to 1", call. = FALSE)
    }
    if (pivot < 0) {
      pivot <- 0
      warning("pivot less than 0, setting it to 0", call. = FALSE)
    }
    mag <- data$norm_mag

    if ("simpleUnit" %in% class(unit(1, "mm"))) {
      arrow$length <- mag*arrow$length
    } else {
      arrow$length <- unit(as.numeric(arrow$length)*mag,
                           attr(arrow$length, "unit"))
    }

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
                       angle = coords$angle,
                       length = unit(coords$mag, "cm"),
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
  compute_group = function(self, data, scales,
                           skip.x = skip.x, skip.y = skip.y,
                           min.mag = min.mag, start = 0, direction = -1,
                           preserve.dir = TRUE, ...) {
    data
  },
  setup_data = function(data, params) {


    params$direction <- switch(params$direction[1],
                               ccw = -1,
                               cw = 1,
                               stop("direction must be either ccw or cw", call. = FALSE)
    )
    if (is.null(data$mag) | is.null(data$angle)) {
      if (is.null(data$dx) | is.null(data$dy)) {
        stop("stat_arrow needs wither mag and angle or dx and dy", call. = FALSE)
      }
      data$mag <- with(data, Mag(dx, dy))
      data$angle <- with(data, atan2(dy, dx)*180/pi)
    } else {
      # Turn into mathematical angle
      data$angle <-  params$start - data$angle*params$direction
      data$dx <- with(data, mag*cos(angle*pi/180))
      data$dy <- with(data, mag*sin(angle*pi/180))
    }

    data <- subset(data, x %in% JumpBy(sort(unique(x)), params$skip.x + 1) &
                     y %in% JumpBy(sort(unique(y)), params$skip.y + 1) &
                     mag >= params$min.mag)
    if (nrow(data) > 0){
      data$norm_mag <- with(data, mag/max(mag, na.rm = TRUE))
    }

    data
  },
  compute_panel = function(self, data, scales,
                           skip.x = 0, skip.y = 0,
                           min.mag = 0, start = 0, direction = -1,
                           preserve.dir = TRUE, ...) {
    if (plyr::empty(data)) return(data.frame())

    groups <- split(data, data$group)
    stats <- lapply(groups, function(group) {
      self$compute_group(data = group, scales = scales, ...)
    })

    stats <- mapply(function(new, old) {
      if (plyr::empty(new)) return(data.frame())
      unique <- ggplot2:::uniquecols(old)
      missing <- !(names(unique) %in% names(new))
      cbind(
        new,
        unique[rep(1, nrow(new)), missing,drop = FALSE]
      )
    }, stats, groups, SIMPLIFY = FALSE)

    data <- do.call(plyr::rbind.fill, stats)

    min.mag <- data$min.mag %||% min.mag

    # Warnings for good usage
    if (preserve.dir == FALSE) {
      if (scales$x$is_discrete() | scales$y$is_discrete()) {
        warning("the use of preserve.dir = FALSE with discrete scales is not recommended", call. = FALSE)
      }

      if (scales$x$scale_name == "date" | scales$x$scale_name == "date") {
        warning("the use of preserve.dir = FALSE with date scales is not recommended", call. = FALSE)
      }
    }

    data
  }
)
