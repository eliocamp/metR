#' Cross pattern
#'
#' Reduces the density of a regular grid using a cross pattern.
#'
#' @param x,y x and y points that define a regular grid.
#' @param skip how many points to skip. Greater value reduces the final point density.
#'
#' @return
#' `is.cross` returns a logical vector indicating whether each point belongs to the
#' reduced grid or not.
#' `cross` returns a list of x and y components of the reduced density grid.
#'
#' @examples
#' # Basic usage
#' grid <- expand.grid(x = 1:10, y = 1:10)
#' cross <- is.cross(grid$x, grid$y, skip = 2)
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
is.cross <- function(x, y, skip = 0) {
    # browser()

    x[!(x %in% JumpBy(unique(x), by = skip + 1))] <- NA
    y[!(y %in% JumpBy(unique(y), by = skip + 1))] <- NA


    x_r <- data.table::frank(x, ties.method = "dense", na.last = "keep")
    y_r <- data.table::frank(y, ties.method = "dense", na.last = "keep")

    # x_r[!(x_r %in% JumpBy(unique(x_r), by = skip + 1))] <- NA
    # y_r[!(y_r %in% JumpBy(unique(y_r), by = skip + 1))] <- NA

    test <- (x_r + y_r) %% 2

    !(test != 0 | is.na(test))

}

#' @export
#' @rdname is.cross
cross <- function(x, y) {
    out <- is.cross(x, y)
    return(list(x = x[out],
                y = y[out]))
}
