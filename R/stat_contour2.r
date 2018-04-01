#' @inheritParams ggplot2::stat_identity
#' @param breaks One of:
#'   - A numeric vector of breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
#' @param circular either NULL, "x" or "y" indicating which dimension is circular,
#' if any.
#' @export
#' @section Computed variables:
#' \describe{
#'  \item{level}{height of contour}
#' }
#' @rdname geom_contour2
#' @family ggplot2 helpers
stat_contour2 <- function(mapping = NULL, data = NULL,
                         geom = "contour", position = "identity",
                         ...,
                         breaks = scales::fullseq,
                         bins = NULL,
                         binwidth = NULL,
                         na.rm = FALSE,
                         circular = NULL,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatContour2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      circular = circular,
      ...
    )
  )
}

StatContour2 <- ggplot2::ggproto("StatContour2", Stat,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(order = ..level..),

  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = scales::fullseq, complete = FALSE,
                           na.rm = FALSE, circular = NULL) {
      # Check is.null(breaks) for backwards compatibility
      if (is.null(breaks)) {
          breaks <- scales::fullseq
      }

      if (is.function(breaks)) {
          # If no parameters set, use pretty bins to calculate binwidth
          if (is.null(bins) && is.null(binwidth)) {
              binwidth <- diff(pretty(range(data$z), 10))[1]
          }
          # If provided, use bins to calculate binwidth
          if (!is.null(bins)) {
              binwidth <- diff(range(data$z)) / bins
          }

          breaks <- breaks(range(data$z), binwidth)
      }

      if (!is.null(circular)) {
          M <- max(data[[circular]]) + resolution(data[[circular]])
          data <- RepeatLon(data, colname = circular, maxlon = M)
      }
      .contour_lines(data, breaks, complete = complete)
    }
)

.contour_lines <- function(data, breaks, complete = FALSE) {
  z <- tapply(data$z, data[c("x", "y")], identity)

  if (is.list(z)) {
    stop("Contour requires single `z` at each combination of `x` and `y`.",
         call. = FALSE)
  }

  cl <- grDevices::contourLines(
    x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
    levels = breaks)

  if (length(cl) == 0) {
    warning("Not possible to generate contour data", call. = FALSE)
    return(data.frame())
  }

  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")

  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
}



StatTextContour <- ggplot2::ggproto("StatTextContour", StatContour2,
                                    required_aes = c("x", "y", "z"),
                                    default_aes = ggplot2::aes(order = ..level.., label = ..level..)
)

