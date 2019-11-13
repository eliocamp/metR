#' Wrap periodic data to any range
#'
#' Periodic data can be defined only in one period and be extended to any arbitrary
#' range.
#'
#' @param x a data.frame
#' @param circular the name of the circular dimension
#' @param wrap the wrap for the data to be extended to
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
#' # But using WrapCircular solves it.
#' g %+% WrapCircular(geopotential[date == date[1]], "lon", c(0, 360))
#'
#' # Aditionally data can be just repeatet to the right and
#' # left
#' ggplot(WrapCircular(geopotential[date == date[1]], wrap = c(-180, 360 + 180)),
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
WrapCircular <- function(x, circular = "lon", wrap = c(0, 360)) {
    warning("WrapCircular is deprecated, use ggperiodic::wrap instead")

    checks <- makeAssertCollection()
    assertDataFrame(x, add = checks)
    assertCharacter(circular, len = 1, any.missing = FALSE, add = checks)
    assertNumeric(wrap, len = 2)
    reportAssertions(checks)

    if (nrow(x) == 0) return(x)

    x <- as.data.table(x)
    data.table::setorderv(x, circular)

    res <- ggplot2::resolution(x[[circular]])
    m <- min(x[[circular]])
    M <- max(x[[circular]])

    # How many steps form the left and right extremes
    # represent the range
    right <- trunc((max(wrap) - M)/res)
    left <- trunc((min(wrap) - m)/res)

    # New "grid"
    x.new <- seq(m + left*res, M + right*res, by = res)

    right <- right + data.table::uniqueN(x[[circular]]) - 1

    # The old coord of the new grid
    index <- seq(left, right)
    index <- index %% length(unique(x[[circular]])) + 1
    x.old <- unique(x[[circular]])[index]

    x.new <- data.table::data.table(x.old, x.new)
    colnames(x.new) <- c(circular, paste0(circular, "new"))

    # Add values according to the old grid and then
    # remove it.
    y <- x[x.new, on = circular, allow.cartesian = TRUE]
    data.table::set(y, NULL, circular, NULL )
    data.table::setnames(y, paste0(circular, "new"), circular)
    return(y)
}

#' @rdname WrapCircular
#' @usage NULL
#' @format NULL
#' @export
RepeatCircular <- function(x, circular = "lon", max = NULL) {
    .Deprecated("WrapCircular")
}
