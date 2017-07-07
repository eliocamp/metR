#' Helpful scales for maps
#'
#' \code{scale_*_longitude} is a wraper around \code[ggplot2]{scale_x_continuous} with
#' helpful defaults for ploting longitude. It can plot labels in the
#' +-180 convention even when data is in the 0-360 convention.
#'
#' @param ticks Spacing between ticks.
#' @rdname scale_longitude
#' @family ggplo2 helpers
#' @export
scale_x_longitude <- function(ticks = 60, name = "", ...) {
    scale_x_continuous(name = name, expand = c(0, 0),
                       breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks),
                                  seq(-180 + ticks, 0 - ticks, by = ticks)),
                       ...)
}

#' @rdname scale_longitude
#' @export
scale_y_longitude <- function(ticks = 60, name = "", ...) {
    scale_y_continuous(name = name, expand = c(0, 0),
                       breaks = seq(0, 360 - ticks, by = ticks),
                       labels = c(seq(0, 180, by = ticks),
                                  seq(-180 + ticks, 0 - ticks, by = ticks)),
                       ...)
}
