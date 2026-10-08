#' Functions for making breaks
#'
#' Functions that return functions suitable to use as the `breaks` argument in
#' ggplot2's continuous scales and in [geom_contour_fill].
#'
#' @param binwidth width of breaks
#' @param bins number of bins, used if `binwidth = NULL`
#' @param exclude a vector of breaks to exclude
#' @param anchor anchor value
#'
#' @return
#' A function that takes a range as argument and a binwidth as an optional argument
#' and returns a sequence of equally spaced intervals covering the range.
#' For `robust_breaks`, a function to use as `breaks` argument in contour geoms. 
#' It takes a data frame containing a column `z`, `binwidth` and `bin` 
#' arguments and returns a numeric vector of breaks, possibly including 
#' `-Inf` and `Inf`.
#'
#' @details
#' `MakeBreaks` is essentially an export of the default way
#' [ggplot2::stat_contour] makes breaks.
#'
#' `AnchorBreaks` makes breaks starting from an `anchor` value and covering
#' the range of the data according to `binwidth`.
#'
#' @examples
#'
#' my_breaks <- MakeBreaks(10)
#' my_breaks(c(1, 100))
#' my_breaks(c(1, 100), 20)    # optional new binwidth argument ignored
#'
#' MakeBreaks()(c(1, 100), 20)  # but is not ignored if initial binwidth is NULL
#' @examplesIf requireNamespace("reshape2")
#' # One to one mapping between contours and breaks
#' library(ggplot2)
#' binwidth <- 20
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour(aes(color = after_stat(level)), binwidth = binwidth) +
#'     scale_color_continuous(breaks = MakeBreaks(binwidth))
#'
#' #Two ways of getting the same contours. Better use the second one.
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour2(aes(color = after_stat(level)), breaks = AnchorBreaks(132),
#'                   binwidth = binwidth) +
#'     geom_contour2(aes(color = after_stat(level)), breaks = AnchorBreaks(132, binwidth)) +
#'     scale_color_continuous(breaks = AnchorBreaks(132, binwidth))
#'
#' # Sample data with extremes
#' surface <- reshape2::melt(volcano)
#' surface$value[sample(nrow(surface), 20)] <- 1000
#' 
#' # With regular breaks the outliers stretch the scale
#' # and prevent seeing the variability of the bulk of the data
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill()
#' 
#' # Robust breaks removes those points from the break 
#' # computation and allow to see the variability
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   geom_contour_fill(breaks = robust_breaks())
#' 
#' @export
#' @family ggplot2 helpers
MakeBreaks <- function(binwidth = NULL, bins = 10, exclude = NULL) {
  force(binwidth)
  force(bins)
  force(exclude)
  function(range, binwidth2 = NULL) {
    if (!is.null(binwidth)) {
      binwidth2 <- binwidth
    }

    # If no parameters set, use pretty bins
    if (is.null(binwidth2)) {
      b <- pretty(range, bins)
      return(b[!(b %in% exclude)])
    } else {
      b <- scales::fullseq(range, binwidth2)
      b[!(b %in% exclude)]
    }
  }
}

#' @rdname MakeBreaks
#' @export
AnchorBreaks <- function(
  anchor = 0,
  binwidth = NULL,
  exclude = NULL,
  bins = 10
) {
  force(anchor)
  force(binwidth)
  force(exclude)
  force(bins)
  function(x, binwidth2 = NULL) {
    if (!is.null(binwidth)) {
      binwidth2 <- binwidth
    }
    if (is.null(binwidth2)) {
      binwidth2 <- diff(pretty(x, bins))[1]
    }

    mult <- ceiling((x[1] - anchor) / binwidth2) - 1L
    start <- anchor + mult * binwidth2
    b <- seq(start, x[2] + binwidth2, binwidth2)
    b[!(b %in% exclude)]
  }
}


#' Robust contour breaks
#'
#' Modifies a breaks function to compute breaks using a trimmed range 
#' of the data to avoid extreme outliers stretching the scale. 
#'
#' @param breaks A function that takes the range of the data and binwidth as input 
#'  and returns the the breaks as output.
#' @param quantiles Numeric vector of length 2, sorted in ascending order,
#'   with values between 0 and 1 representing the quantiles of `z` that 
#'   define the range used to compute breaks.
#' 
#' @export
#' @rdname MakeBreaks
robust_breaks <- function(
  breaks = MakeBreaks(),
  quantiles = c(0.02, 0.98)
) {
  force(breaks)
  checkmate::assert_numeric(
    quantiles,
    lower = 0,
    upper = 1,
    any.missing = FALSE,
    len = 2,
    sorted = TRUE
  )

  fun <- function(data, binwidth = NULL, bins = NULL) {
    x <- data$z
    r <- quantile(x, quantiles, na.rm = TRUE)
    
    if (is.null(bins) && is.null(binwidth)) {
      binwidth <- diff(pretty(r, 10))[1]
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(r) / bins
    }

    breaks <- breaks(r, binwidth)

    if (min(x, na.rm = TRUE) < breaks[1]) {
      breaks <- c(-Inf, breaks)
    }
    if (max(x, na.rm = TRUE) > breaks[length(breaks)]) {
      breaks <- c(breaks, Inf)
    }
    
    return(breaks)
  }
  class(fun) <- "metR_breaks_fulldata"
  return(fun)
}