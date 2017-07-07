#' Repeat minimum longitude
#'
#' ggplot's \code{\link[ggplot2]{coord_polar}} doesn't close contours in cyclical data so
#' the workaround is to repeat the leftmost data.
#'
#' @param x A data.table (or a data.frame which will be converted into a
#' data.table).
#' @param colname The name of the longitude-like column.
#'
#' @return A data.table with the values corresponding to the minimum longitude
#' repeated next to the maximum.
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' field <- setDT(expand.grid(lat = seq(-90, 0, by = 3.5),
#'                            lon = seq(0, 360, by = 3.5)))
#' field$z <- with(field, sin(lat*pi/180*4) + cos(lon*pi/180*5))
#' ggplot(RepeatLon(field), aes(lon, lat)) +
#'     geom_contour(aes(z = z)) +
#'     coord_polar()
#' @family ggplo2 helpers
#' @export
RepeatLon <- function(x, colname = "lon") {
    setDT(x)
    minlon = x[, min(get(colname))]
    # maxlon = x[, max(get(colname))]
    border <- x[get(colname) == minlon, ]
    border[, get(colname) := 360 + minlon]
    rbind(x, border)
}


# ggplot(RepeatLon(field), aes(lon, lat)) +
#     geom_contour(aes(z = z)) +
#     coord_polar()

