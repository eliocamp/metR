#' Arrows
#'
#' Unlinke [geom_vector], `geom_arrow()` preserves direction in coordinate
#' transformation and different aspect ratios.
#'
#' @inheritParams ggplot2::geom_text
#' @param direction Direction of rotation.
#' @param start Starting angle for rotation in radians.
#'
#' @details
#' Direction and start allows to work with different standards. For the
#' meteorological direction, for example, use `direction = -1/4*pi`.
#'
#' @section Aesthetics:
#' `geom_vector` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **mag**
#' \item **angle**
#' \item alpha
#' \item colour
#' }
#'
#' @export
#' @family ggplot2 helpers
geom_arrow <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       direction = 1,
                       start = 0,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,...) {
    angle <- deparse(mapping$angle)
    m <- aes_string(angle = paste0(start,  "+", direction, "*", angle))
    mapping$angle <- m$angle
    mapping$size <- mapping$mag
    mapping$mag <- NULL
    mapping$label <- "\u27f6"
geom
    layer(geom = GeomText,
          mapping = mapping,
          data = data,
          stat = stat,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
    )
}
