#' Divergent color scales
#'
#' Wraper around ggplot's \code{\link[ggplot2]{scale_colour_gradient2}} with better defaults and
#' automatic annotated breaks. It's main use it to label the same levels as
#' the contours in a plot.
#'
#' @param binwidth (optional) Binwidth for computing breaks. For best resutls,
#' should be the same value as the related \code{stat_contour} call.
#' @inheritParams ggplot2::scale_colour_gradient2
#'
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour(binwidth = 30, aes(color = ..level..)) +
#'   scale_color_divergent(binwidth = 30)
#'
#' @rdname scale_divergent
#' @family ggplot2 helpers
#' @export
#' @import ggplot2 scales
scale_color_divergent <- function(low = scales::muted("blue"), high = scales::muted("red"),
                                  binwidth = NA, ...) {
    # Escala divergente con defaults más razonables.
    if (!is.na(binwidth)) {
        breaks <- function(x){
            c(seq(x[1], 0 - binwidth, by = -binwidth), seq(0, x[2], by = binwidth))
        }
        return(ggplot2::scale_color_gradient2(low = low, high = high, breaks = breaks, ...))
    } else {
        return(ggplot2::scale_color_gradient2(low = low, high = high, ...))
    }
}

#' @rdname scale_divergent
#' @export
#' @import ggplot2 scales
scale_fill_divergent <- function(low = scales::muted("blue"), high = scales::muted("red"),
                                 binwidth = NA, ...) {
    # Escala divergente con defaults más razonables.
    if (!is.na(binwidth)) {
        breaks <- function(x){
            c(seq(x[1], 0 - binwidth, by = -binwidth), seq(0, x[2], by = binwidth))
        }
        return(ggplot2::scale_fill_gradient2(low = low, high = high, breaks = breaks, ...))
    } else {
        return(ggplot2::scale_fill_gradient2(low = low, high = high, ...))
    }
}
