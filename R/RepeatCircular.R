#' Repeat minimum longitude
#'
#' ggplot's \code{\link[ggplot2]{coord_polar}} doesn't close contours in
#' cyclical data so the workaround is to copy the leftmost data to the right.
#'
#' @param x a data.frame
#' @param circular the name of the circular dimension
#' @param max the value of the longitude-like column that the duplicated
#' values should have. If `NULL`, defaults to max + resolution.
#'
#' @return
#' A data.frame with the values corresponding to the minimum longitude
#' repeated next to the maximum.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' data(geopotential)
#' g <- ggplot(geopotential[date == date[1]], aes(lon, lat)) +
#'     geom_contour(aes(z = gh)) +
#'     coord_polar() +
#'     ylim(c(-90, -10))
#'
#' # This plot has problems in lon = 0
#' g
#'
#' # But using RepeatCircular solves it.
#' g %+% RepeatCircular(geopotential[date == date[1]])
#'
#' # The same behaviour is now implemented directly in geom_contour2
#' ggplot(geopotential[date == date[1]], aes(lon, lat)) +
#'     geom_contour2(aes(z = gh), circular = "x") +
#'     coord_polar() +
#'     ylim(c(-90, -10))
#'
#' @seealso geom_contour2
#'
#' @family ggplot2 helpers
#' @export
#' @import data.table
RepeatCircular <- function(x, circular = "lon", max = NULL) {
    dt <- data.table::is.data.table(x)
    data.table::setDT(x)
    minlon <- x[, min(get(circular))]
    border <- x[get(circular) == minlon, ]
    max <- max(x[[circular]]) + ggplot2::resolution(x[[circular]])
    border[, c(circular) := max]
    full <- rbind(x, border)
    if (dt == TRUE) {
        return(full)
    } else {
        return(setDF(full))
    }
}

