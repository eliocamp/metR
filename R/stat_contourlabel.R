#' Label contours
#'
#' Draws labels on contours built with [ggplot2::stat_contour].
#'
#' @inheritParams ggplot2::geom_contour
#' @param min.size minimum number of points for a contour to be labeled.
#' @param skip number of contours to skip
#'
#' @details
#' Is best used with a previous call to [ggplot2::stat_contour] with the same
#' parameters.
#'
#' @examples
#' library(ggplot2)
#' v <- data.table::melt(volcano)
#' ggplot(v, aes(Var1, Var2)) +
#'    geom_contour(aes(z = value)) +
#'    stat_contourlabel(aes(z = value), skip = 0)
#'
#' # Small hack to avoid overlap between contours and labels
#' geom_contourlabel <- function(...) {
#'    list(stat_contourlabel(geom = "label", fill = "white",
#'                           label.r = unit(0, "lines"),
#'                           label.padding = unit(0.04, "lines"), color = NA, ...),
#'         stat_contourlabel(...))
#'  }
#'
#' ggplot(v, aes(Var1, Var2)) +
#'    geom_contour(aes(z = value)) +
#'    geom_contourlabel(aes(z = value), skip = 1)
#'
#' @section Aesthetics:
#' \code{geom_contourlabel} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{z}
#' \item \code{label}
#' \item \code{alpha}
#' \item \code{angle}
#' \item \code{colour}
#' \item \code{family}
#' \item \code{fontface}
#' \item \code{group}
#' \item \code{hjust}
#' \item \code{lineheight}
#' \item \code{size}
#' \item \code{vjust}
#'}
#'
#'
#' @export
#' @import ggplot2 data.table
#' @family ggplot2 helpers
stat_contourlabel <- function(mapping = NULL, data = NULL,
                         geom = "text", position = "identity",
                         ...,
                         min.size = 6,
                         skip = 1,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatContourLabel,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            min.size = min.size,
            skip = skip,
            ...
        )
    )
}

StatContourLabel <- ggproto("StatContourLabel", Stat,
    required_aes = c("x", "y", "z"),
    default_aes = aes(label = ..level..),
    compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                             breaks = NULL, complete = FALSE, na.rm = FALSE,
                             skip = 1, exclude = NA, include = NA, min.size = 6) {
        # If no parameters set, use pretty bins
        if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
            breaks <- pretty(range(data$z), 10)
            }
        # If provided, use bins to calculate binwidth
        if (!is.null(bins)) {
            binwidth <- diff(range(data$z)) / bins
            }
        # If necessary, compute breaks from binwidth
        if (is.null(breaks)) {
            breaks <- scales::fullseq(range(data$z), binwidth)
            }
        breaks.keep <- breaks[seq(1, length(breaks), by = skip + 1)]
        breaks.keep <- breaks.keep[!(breaks.keep %in% exclude)]
        breaks.keep <- c(breaks.keep, include)
        breaks.keep <- breaks.keep[!is.na(breaks.keep)]

        contours <- ggplot2:::contour_lines(data, breaks.keep, complete = complete)
        contours.dt <- data.table::as.data.table(contours)
        contours.dt[, N := .N, by = piece]
        contours.dt <- contours.dt[N >= min.size]
        contours.dt[, var := minvar(x, y), by = .(piece)]

        as.data.frame(contours.dt[var == T][, head(.SD, 1), by = piece])
        }
)

# from https://stackoverflow.com/questions/21868353/drawing-labels-on-flat-section-of-contour-lines-in-ggplot2
minvar <- function (x, y){
    N <- length(x)
    xdiffs <- x[2:N] - x[1:(N-1)]
    ydiffs <- y[2:N] - y[1:(N-1)]
    avgGradient <- ydiffs/xdiffs
    squareSum <- avgGradient * avgGradient
    variance <- (squareSum - (avgGradient * avgGradient) / N / N)
    variance <- c(NA, NA, variance[3:(N-2)], NA, NA)
    return(variance == min(variance, na.rm = T))
}


geom_contourlabel <- function(...) {
    list(stat_contourlabel(geom = "label", fill = "white", label.r = unit(0, "lines"),
                      label.padding = unit(0.04, "lines"), color = NA, ...),
        stat_contourlabel(...))
}

