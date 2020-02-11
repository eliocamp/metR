#' Divergent colour scales
#'
#' Wrapper around ggplot's \code{\link[ggplot2]{scale_colour_gradient2}} with
#' inverted defaults of `high` and `low`.
#'
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @examples
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'   geom_contour(aes(color = ..level..)) +
#'   scale_colour_divergent(midpoint = 130)
#'
#' @name scale_divergent
#' @family ggplot2 helpers
#' @export
scale_colour_divergent <- function(..., low = scales::muted("blue"), mid = "white",
                                  high = scales::muted("red"), midpoint = 0, space = "Lab",
                                  na.value = "grey50", guide = "colourbar") {
    ggplot2::scale_color_gradient2(..., low = low, high = high, mid = mid, midpoint = midpoint,
                                   space = space, na.value = na.value, guide = guide)
}

#' @export
#' @rdname scale_divergent
scale_color_divergent <- scale_colour_divergent

#' @rdname scale_divergent
#' @export
scale_fill_divergent <- function(..., low = scales::muted("blue"),
                                 mid = "white",
                                 high = scales::muted("red"),
                                 midpoint = 0,
                                 space = "Lab",
                                 na.value = "grey50",
                                 guide = "colourbar") {
    ggplot2::scale_fill_gradient2(..., low = low, high = high, mid = mid,
                                  midpoint = midpoint, space = space,
                                  na.value = na.value, guide = guide)
}

