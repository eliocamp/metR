#' 2d contours of a 3d surface
#'
#' A copy of [ggplot2::geom_contour] that accepts a function as the `breaks`
#' argument.
#'
#' @section Aesthetics:
#' \code{geom_contour2} understands the following aesthetics (required aesthetics are in bold):
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
#' @examples
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, color = ..level..),
#'                   breaks = AnchorBreaks(130, binwidth = 11))
#'
#' @inheritParams ggplot2::geom_contour
#' @export
geom_contour2 <- function(mapping = NULL, data = NULL,
                         stat = "contour2", position = "identity",
                         ...,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 1,
                         breaks = scale::fullseq,
                         bins = NULL,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomContour,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      ...
    )
  )
}
