#' Repeat minimum longitude
#'
#' ggplot's \code{\link[ggplot2]{coord_polar}} doesn't close contours in
#' cyclical data so the workaround is to copy the leftmost data to the right.
#'
#' @param x a data.frame
#' @param colname the name of the longitude-like column
#' @param maxlon the value of the longitude-like column that the duplicated
#' values should have.
#'
#' @return
#' A data.frame with the values corresponding to the minimum longitude
#' repeated next to the maximum.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' g <- ggplot(aao[date == date[1]], aes(lon, lat)) +
#'     geom_contour(aes(z = gh)) +
#'     coord_polar() +
#'     ylim(c(-90, -10))
#'
#' # This plot has problems in lon = 0
#' g
#'
#' # But using RepeatLon solves it.
#' g %+% RepeatLon(aao[date == date[1]])
#' @family ggplot2 helpers
#' @export
#' @import data.table
RepeatLon <- function(x, colname = "lon", maxlon = 360) {
    dt <- data.table::is.data.table(x)
    data.table::setDT(x)
    minlon <- x[, min(get(colname))]
    border <- x[get(colname) == minlon, ]
    border[, c(colname) := maxlon]
    full <- rbind(x, border)
    if (dt == TRUE) {
        return(full)
    } else {
        return(setDF(full))
    }
}
