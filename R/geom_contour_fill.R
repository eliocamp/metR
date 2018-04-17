#' Filled 2d contours of a 3d surface
#'
#' While ggplot2's \code{\link[ggplot2]{geom_contour}} can plot nice contours, it
#' doesn't work with the polygon geom. This stat makes some small manipulation
#' of the data to ensure that all contours are closed and also computes a new
#' aesthetic \code{int.level}, which differs from \code{level} (computed by
#' [ggplot2::geom_contour]) in that represents
#' the value of the \code{z} aesthetic *inside* the contour instead of at the edge.
#'
#' @inheritParams ggplot2::geom_contour
#' @param breaks numeric vector of breaks
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
#' @param circular either NULL, "x" or "y" indicating which dimension is circular,
#' if any.
#'
#' @section Aesthetics:
#' \code{geom_contour_fill} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
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
                         breaks = NULL,
                         bins = NULL,
                         binwidth = NULL,
                         na.rm = FALSE,
                         circular = NULL,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPolygon,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            na.rm = na.rm,
            circular = circular,
            ...
        )
    )
}
