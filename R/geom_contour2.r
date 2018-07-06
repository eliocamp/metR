#' 2d contours of a 3d surface
#'
#' A copy of [ggplot2::geom_contour] that accepts a function as the `breaks`
#' argument and makes gaps for labels and computes breaks globally instead of
#' per panel.
#'
#' @inheritParams ggplot2::geom_contour
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
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, color = ..level..),
#'                   breaks = AnchorBreaks(130, binwidth = 11))
#'
#' @family ggplot2 helpers
#' @export
geom_contour2 <- function(mapping = NULL, data = NULL,
                          stat = "contour2", position = "identity",
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 1,
                          breaks = scales::fullseq,
                          bins = NULL,
                          binwidth = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    dots <- list(...)
    if (!is.null(dots$gap)) stop("gap argument in geom_contour2 is deprecated, use stroke in geom_contour_text instead",
                                 call. = FALSE)

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContour2,
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


#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
GeomContour2 <- ggplot2::ggproto("GeomContour2", ggplot2::GeomContour,
   default_aes = aes(weight = 1, colour = "black", size = 0.5, linetype = 1,
                     alpha = NA))
