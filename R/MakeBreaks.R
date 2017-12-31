#' Make breaks for contours
#'
#' Essencially an export of the default way [ggplot2::stat_contour] makes breaks,
#' it's intended to use as the `breaks` argument of [ggplot2::scale_color_continuous] or
#' [ggplot2::scale_fill_continuous]. This way, there's a one to one mapping
#' between contours and breaks.
#'
#' @param binwidth width of breaks
#' @param bins number of bins, used if `binwidth = NULL`
#' @param exclude a vector of breaks to exclude
#'
#' @return
#' A function that takes a range as argument and returns a sequence of equally
#' spaced intervals covering the range.
#'
#' @examples
#'
#' my_breaks <- MakeBreaks(10)
#' my_breaks(range(1:100))
#'
#' library(ggplot2)
#' binwidth <- 20
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour(aes(color = ..level..), binwidth = binwidth) +
#'     scale_color_continuous(breaks = MakeBreaks(binwidth))
#'
#' @export
#' @family ggplot2 helpers
MakeBreaks <- function(binwidth = NULL, bins = 10, exclude = NULL) {
    # If no parameters set, use pretty bins
    if (is.null(binwidth)) {
        breaks <- function(range) {
            b <- pretty(range, bins)
            b[!(b %in% exclude)]
        }
    } else {
        breaks <- function(range) {
            b <- scales::fullseq(range, binwidth)
            b[!(b %in% exclude)]
        }
    }
}
