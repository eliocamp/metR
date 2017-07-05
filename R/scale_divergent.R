#' Divergent color scales
#'
#' Wraper around ggplot's \code{scale_*_gradient2} with better defaults and
#' automatic annotated breaks. It's main use it to label the same levels as
#' the contours in a plot.
#'
#' @param binwidth (optional) Binwidth for computing breaks. For best resutls,
#' should be the same value as the related \code{stat_contour} call.
#'
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' gplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour(binwidth = 30, aes(color = ..level..)) +
#'   scale_color_divergent(binwidth = 30)
#' @export
scale_color_divergent <- function(low = muted("blue"), high = muted("red"), binwidth = NA, ...) {
    # Escala divergente con defaults más razonables.
    if (!is.na(binwidth)) {
        breaks <- function(x){
            c(seq(x[1], 0 - binwidth, by = -binwidth), seq(0, x[2], by = binwidth))
        }
        return(scale_color_gradient2(low = low, high = high, breaks = breaks, ...))
    } else {
        return(scale_color_gradient2(low = low, high = high, ...))
    }
}

#' @export
scale_fill_divergent <- function(low = muted("blue"), high = muted("red"), binwidth = NA, ...) {
    # Escala divergente con defaults más razonables.
    if (!is.na(binwidth)) {
        breaks <- function(x){
            c(seq(x[1], 0 - binwidth, by = -binwidth), seq(0, x[2], by = binwidth))
        }
        return(scale_fill_gradient2(low = low, high = high, breaks = breaks, ...))
    } else {
        return(scale_fill_gradient2(low = low, high = high, ...))
    }
}
