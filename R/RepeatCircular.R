#' Extend periodic data to any range
#'
#' Periodic data can be defined only in one period and be extended to any arbitrary
#' range.
#'
#' @param x a data.frame
#' @param circular the name of the circular dimension
#' @param range the range for the data to be extended to
#'
#' @return
#' A data.frame.
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
#' g %+% ExtendCircular(geopotential[date == date[1]], "lon", c(0, 360))
#'
#' # Aditionally data can be just repeatet to the right and
#' # left
#' ggplot(ExtendCircular(geopotential[date == date[1]], range = c(-180, 360 + 180)),
#'        aes(lon, lat)) +
#'     geom_contour(aes(z = gh))
#'
#' # The same behaviour is now implemented directly in geom_contour2
#' # and geom_contour_fill
#' ggplot(geopotential[date == date[1]], aes(lon, lat)) +
#'     geom_contour2(aes(z = gh), xwrap = c(-180, 360 + 180))
#'
#' @seealso geom_contour2
#'
#' @family ggplot2 helpers
#' @export
#' @import data.table
ExtendCircular <- function(x, circular = "lon", range = c(0, 360)) {
    setDT(x)
    res <- ggplot2::resolution(x[[circular]])
    m <- min(x[[circular]])
    M <- max(x[[circular]])

    # How many steps form the left and right extremes
    # represent the range
    right <- trunc((max(range) - M)/res)
    left <- trunc((min(range) - m)/res)

    # New "grid"
    x.new <- seq(m + left*res, M + right*res, by = res)

    right <- right + uniqueN(x[[circular]]) - 1

    # The old coord of the new grid
    index <- seq(left, right)
    index <- index %% length(unique(x[[circular]])) + 1
    x.old <- unique(x[[circular]])[index]

    x.new <- data.table(x.old, x.new)
    colnames(x.new) <- c(circular, paste0(circular, "new"))

    # Add values according to the old grid and then
    # remove it.
    y <- x[x.new, on = circular, allow.cartesian = TRUE]
    set(y, NULL, circular, NULL )
    setnames(y, paste0(circular, "new"), circular)
    return(y)
}

#' @export
#' @rdname ExtendCircular
RepeatCircular <- function(x, circular = "lon", max = NULL) {
    .Deprecated("ExtendCircular")
}
