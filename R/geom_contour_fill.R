#' Filled 2d contours of a 3d surface
#'
#' While ggplot2's \code{\link[ggplot2]{geom_contour}} can plot nice contours, it
#' doesn't work with the polygon geom. This stat makes some small manipulation
#' of the data to ensure that all contours are closed and also computes a new
#' aesthetic \code{int.level}, which differs from \code{level} (computed by
#' [ggplot2::gemom_contour]) in that represents
#' the value of the \code{z} aesthetic *inside* the contour instead of at the edge.
#'
#' @inheritParams ggplot2::geom_contour
#' @param exclude a numeric vector of levels that should be excluded from the
#' contour calculations
#'
#' @section Aesthetics:
#' \code{geom_contour_fill} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \code{\strong{x}}
#' \item \code{\strong{y}}
#'  \item \code{alpha}
#'  \item \code{colour}
#'  \item \code{group}
#'  \item \code{linetype}
#'  \item \code{size}
#'  \item \code{weight}
#'}
#'
#'
#' @section Computed variables:
#' \describe{
#'  \item{int.level}{value of the interior contour}
#'  }
#'
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill() +
#'   geom_contour(color = "black", size = 0.1)
#'
#' # Plots only deviations from the mean.
#' ggplot(surface, aes(Var1, Var2, z = as.numeric(scale(value)))) +
#'   geom_contour_fill(complete = FALSE, exclude = 0)
#'
#' # If one uses level instead of int.level, one of the small
#' # contours near the crater disapears
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill(aes(fill = ..level..))
#'
#'
#'
#' @family ggplot2 helpers
#' @export
#' @import sp
#' @import ggplot2
geom_contour_fill <- function(mapping = NULL, data = NULL,
                         stat = "ContourFill", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContourFill,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}


GeomContourFill <- ggplot2::ggproto("GeomContourFill", GeomPolygon,
                       default_aes = ggplot2::aes(weight = 1, colour = NA, size = 0.5, linetype = 1,
                                         alpha = NA, fill = int.level)
                       )
