#' Filled 2d contours of a 3d surface
#'
#' While ggplot2's \code{\link[ggplot2]{geom_contour}} can plot nice contours, it
#' doesn't work with the polygon geom. This stat makes some small manipulation
#' of the data to ensure that all contours are closed and also computes a new
#' aesthetic \code{int.level}, which differs from \code{level} (computed by
#' [ggplot2::geom_contour]) in that represents
#' the value of the \code{z} aesthetic *inside* the contour instead of at the edge.
#' It also computes breaks globally instead of per panel, so that faceted plots
#' have all the same binwidth.
#'
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_contour2
#' @param breaks numeric vector of breaks
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
#' @param na.fill How to fill missing values.
#'    - `FALSE` for letting the computation fail with no interpolation
#'    - `TRUE` for imputing missing values with [Impute2D]
#'    - A numeric value for constant imputation
#'    - A function that takes a vector and returns a numeric (e.g. `mean`)
# #' @param xwrap,ywrap vector of length two used to wrap the circular dimension.
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
#' # If one uses level instead of int.level, one of the small
#' # contours near the crater disapears
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill(aes(fill = ..level..))
#'
#' @family ggplot2 helpers
#' @export
#' @import ggplot2
geom_contour_fill <- function(mapping = NULL, data = NULL,
                         stat = "ContourFill", position = "identity",
                         ...,
                         breaks = MakeBreaks(),
                         bins = NULL,
                         binwidth = NULL,
                         # na.rm = FALSE,
                         na.fill = FALSE,
                         # fill.linear = TRUE,
                         # xwrap = NULL,
                         # ywrap = NULL,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    .check_wrap_param(list(...))
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
            na.rm = FALSE,
            na.fill = na.fill,
            # fill.linear = fill.linear,
            # xwrap = xwrap,
            # ywrap = ywrap,
            ...
        )
    )
}


