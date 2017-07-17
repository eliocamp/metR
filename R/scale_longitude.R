#' Helpful scales for maps
#'
#' These functions are simple wrapers around
#' \code{\link[ggplot2]{scale_x_continuous}} and
#' \code{\link[ggplot2]{scale_y_continuous}} with
#' helpful defaults for ploting longitude and latitude.
#'
#' @param ticks spacing between breaks
#' @param name name of the scale
#' @param expand a numeric vector of length two giving multiplicative and
#' additive expansion constants.
#' @param trans either the name of a transformation object, or the object itself
#' @param ... other arguments passed to \code{\link[ggplot2]{scale_x_continuous}}
#'
#' @details
#' scale_*_longitude assumes that your data is between 0 and 360 but labels it
#' from -180 to 180.
#'
#' @name scale_longitude
#' @aliases scale_latitude
#' @examples
#' library(ggplot2)
#' slp <- copy(slp)
#' ggplot(slp, aes(lon, lat, z = slp)) +
#'     geom_contour() +
#'     scale_x_longitude() +
#'     scale_y_latitude()
#'
#' @family ggplot2 helpers
#' @export
scale_x_longitude <- function(ticks = 60, name = "", expand = c(0, 0), ...) {
    scale_x_continuous(name = name, expand = expand,
                       breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks),
                                  seq(-180 + ticks, 0 - ticks, by = ticks)),
                       ...)
}

#' @rdname scale_longitude
#' @export
scale_y_longitude <- function(ticks = 60, name = "", expand = c(0, 0), ...) {
    scale_y_continuous(name = name, expand = expand,
                       breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks),
                                  seq(-180 + ticks, 0 - ticks, by = ticks)),
                       ...)
}


#' @rdname scale_longitude
#' @export
scale_x_latitude <- function(ticks = 30, name = "", expand = c(0, 0), ...) {
    scale_x_continuous(name = name, expand = expand,
                       breaks = seq(-90, 90, by = ticks),
                       ...)
}

#' @rdname scale_longitude
#' @export
scale_y_latitude <- function(ticks = 30, name = "", expand = c(0, 0), ...) {
    scale_y_continuous(name = name, expand = expand,
                       breaks = seq(-90, 90, by = ticks),
                       ...)
}

#' @rdname scale_longitude
#' @export
scale_x_level <- function(name = "", expand = c(0, 0), trans = "reverselog", ...) {
    scale_x_continuous(name = name, expand = expand,
                       trans = trans,
                       ...)
}

#' @rdname scale_longitude
#' @export
scale_y_level <- function(name = "", expand = c(0, 0), trans = "reverselog", ...) {
    scale_y_continuous(name = name, expand = expand,
                       trans = trans,
                       ...)
}

