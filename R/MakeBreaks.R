#' Functions for making breaks
#'
#' Functions that return functions suitable to use as the `breaks` argument in
#' ggplot2's continuous scales.
#'
#' @param binwidth width of breaks
#' @param bins number of bins, used if `binwidth = NULL`
#' @param exclude a vector of breaks to exclude
#' @param anchor anchor value
#'
#' @return
#' A function that takes a range as argument and returns a sequence of equally
#' spaced intervals covering the range.
#'
#' @details
#' `MakeBreaks` is essencially an export of the default way
#' [ggplot2::stat_contour] makes breaks.
#'
#' `AnchorBreaks` makes breaks starting from an `anchor` value and covering
#' the range of the data acording to `binwidth`.
#'
#' @examples
#'
#' my_breaks <- MakeBreaks(10)
#' my_breaks(range(1:100))
#'
#' # One to one mapping between contours and breaks
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


#' @rdname MakeBreaks
#' @export
#' @family ggplot2 helpers
AnchorBreaks <- function(binwidth, anchor = 0) {
    function(x) {
        mult <- ceiling((x[1] - anchor)/binwidth)
        start <- anchor + mult*binwidth
        seq(start, x[2], binwidth)
    }
}

