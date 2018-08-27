#' Helpful scales for maps
#'
#' These functions are simple wrappers around
#' \code{\link[ggplot2]{scale_x_continuous}} and
#' \code{\link[ggplot2]{scale_y_continuous}} with
#' helpful defaults for plotting longitude, latitude and pressure levels.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param ticks spacing between breaks
#'
#'
#' @name scale_longitude
#' @aliases scale_latitude
#' @examples
#' data(geopotential)
#' library(ggplot2)
#' ggplot(geopotential[date == date[1]], aes(lon, lat, z = gh)) +
#'     geom_contour() +
#'     scale_x_longitude() +
#'     scale_y_latitude()
#'
#' @family ggplot2 helpers
#' @export
#' @import ggplot2
scale_x_longitude <- function(name = "", ticks = 30,
                              breaks = seq(0, 360, by = ticks),
                              expand = c(0, 0),
                              labels = LonLabel(breaks),
                              trans = "lon180",
                              ...) {
    labels = waiver()
    ggplot2::scale_x_continuous(name = name, expand = expand,
                                breaks = breaks,
                                labels = labels,
                                trans = trans,
                                ...)
}

#' @rdname scale_longitude
#' @export
#' @import ggplot2
scale_y_longitude <- function(name = "", ticks = 60,
                              breaks = seq(-180, 360, by = ticks),
                              expand = c(0, 0),
                              labels = LonLabel(breaks),
                              trans = "lon180",
                              ...) {
    ggplot2::scale_y_continuous(name = name, expand = expand,
                       breaks = breaks,
                       labels = labels,
                       ...)
}


#' @rdname scale_longitude
#' @export
#' @import ggplot2
scale_x_latitude <- function(name = "", ticks = 30,
                             breaks = seq(-90, 90, by = ticks),
                             expand = c(0, 0),
                             labels = LatLabel(breaks), ...) {
    ggplot2::scale_x_continuous(name = name, expand = expand,
                                breaks = breaks, labels = labels,
                                ...)
}

#' @rdname scale_longitude
#' @export
#' @import ggplot2
scale_y_latitude <- function(name = "", ticks = 30,
                             breaks = seq(-90, 90, by = ticks),
                             expand = c(0, 0),
                             labels = LatLabel(breaks), ...) {
    ggplot2::scale_y_continuous(name = name, expand = expand,
                       breaks = breaks, labels = labels,
                       ...)
}

#' @rdname scale_longitude
#' @export
#' @import ggplot2
scale_x_level <- function(name = "", expand = c(0, 0), trans = "reverselog", ...) {
    ggplot2::scale_x_continuous(name = name, expand = expand,
                       trans = trans,
                       ...)
}

#' @rdname scale_longitude
#' @export
#' @import ggplot2
scale_y_level <- function(name = "", expand = c(0, 0), trans = "reverselog", ...) {
    ggplot2::scale_y_continuous(name = name, expand = expand,
                       trans = trans,
                       ...)
}



#' Label longitude and latitude
#'
#' Provide easy functions for adding suffixes to longitude and latitude for labeling
#' maps.
#'
#' @param lon longitude in degrees
#' @param lat latitude in degrees
#' @param east,west,north,south text to append for each cuadrant
#'
#' @details
#' The default values are for Spanish.
#'
#' @examples
#'
#' LonLabel(0:360)
#'
#' # If you want it to speak in English.
#' LonLabel <- function(...) metR::LonLabel(..., west = "\u00B0W")
#'
#'
#' @export
#' @name map_labels
#' @family ggplot2 helpers
LonLabel <- function(lon, east = "\u00B0E", west = "\u00B0O") {
    lon <- as.numeric(lon)
    lon <- ifelse(lon > 180, ConvertLongitude(lon), lon)
    newlon <- ifelse(lon < 0, paste0(abs(lon), west), paste0(lon, east))
    newlon[lon == 0] <- paste0(lon[lon == 0], "\u00B0")
    newlon[lon == 180] <- paste0(lon[lon == 180], "\u00B0")
    return(newlon)
}

#' @export
#' @rdname map_labels
LatLabel <- function(lat, north = "\u00B0N", south = "\u00B0S") {
    lat <- as.numeric(lat)
    newlat <- ifelse(lat < 0, paste0(abs(lat), south), paste0(lat, north))
    newlat[lat == 0] <- paste0(lat[lat == 0], "\u00B0")
    return(newlat)
}
