#' Filter values
#'
#' Removes values where `filter` evaluates to `FALSE`. Useful for showing only
#' statistical significan values, or a interesting subset of the data without
#' subsetting the data.
#'
#' @inheritParams ggplot2::stat_identity
#'
#' @section Aesthetics:
#' `stat_filter` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **filter**
#' \item `width`
#' \item `height`
#' }
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(data.table::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour(aes(z = value)) +
#'     stat_filter(aes(filter = value >= 150 & value <= 160),
#'                 shape = 3, color = "red")
#'
#' @seealso [stat_na] for a more specialized stat for filtering `NA` values.
#' @export
#' @family ggplot2 helpers
stat_filter <- function(mapping = NULL, data = NULL,
                    geom = "point", position = "identity",
                    ...,
                    show.legend = NA,
                    inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatFilter,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            ...
        )
    )
}

#' @import ggplot2
#' @import scales
StatFilter <- ggplot2::ggproto("StatFilter", ggplot2::Stat,
                           required_aes = c("x", "y", "filter"),
                           compute_group = function(data, scales, width = NULL, height = NULL) {
                               data$width <- data$width %||% width %||% resolution(data$x, FALSE)
                               data$height <- data$height %||% height %||% resolution(data$y, FALSE)
                               data <- subset(data, filter == TRUE)
                               data
                           }
)

