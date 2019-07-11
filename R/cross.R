#' Cross pattern
#'
#' Reduces the density of a regular grid using a cross pattern.
#'
#' @param x,y x and y points that define a regular grid.
#'
#' @return
#' `is.cross` returns a logical vector indicating whether each point belongs to the
#' reduced grid or not.
#' `corss` returns a list of x and y components of the reduced density grid.
#'
#' @examples
#' # Basic usage
#' grid <- expand.grid(x = 1:10, y = 1:10)
#' cross <- is.cross(grid$x, grid$y)
#'
#' with(grid, plot(x, y))
#' with(grid, points(x[cross], y[cross], col = "red"))
#'
#' # Its intended use is to highlight areas with geom_subset()
#' # with reduced densnity. This "hatches" areas with temperature
#' # over 270K
#' library(ggplot2)
#' ggplot(temperature[lev == 500], aes(lon, lat)) +
#'   geom_raster(aes(fill = air)) +
#'   stat_subset(aes(subset = air > 270 & is.cross(lon, lat)),
#'               geom = "point", size = 0.1)
#'
#' @export
is.cross <- function(x, y) {
    x_r <- data.table::frank(x, ties.method = "dense")
    y_r <- data.table::frank(y, ties.method = "dense")

    (x_r + y_r) %% 2 == 0
}

#' @export
#' @rdname is.cross
cross <- function(x, y) {
    out <- is.cross(x, y)
    return(list(x = x[out],
                y = y[out]))
}
