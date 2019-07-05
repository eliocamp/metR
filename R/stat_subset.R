#' Subset values
#'
#' Removes values where `subset` evaluates to `FALSE`. Useful for showing only
#' statistical significant values, or an interesting subset of the data without
#' manually subsetting the data.
#'
#' @inheritParams ggplot2::stat_identity
#'
#' @section Aesthetics:
#' `stat_subset` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **subset**
#' \item `width`
#' \item `height`
#' }
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour(aes(z = value)) +
#'     stat_subset(aes(subset = value >= 150 & value <= 160),
#'                 shape = 3, color = "red")
#'
#' @seealso [stat_na] for a more specialized stat for filtering `NA` values.
#' @export
#' @family ggplot2 helpers
stat_subset <- function(mapping = NULL, data = NULL,
                    geom = "point", position = "identity",
                    ...,
                    show.legend = NA,
                    inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatSubset,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            ...
        )
    )
}

#' @import ggplot2
#' @import scales
#' @rdname stat_subset
#' @usage NULL
#' @format NULL
#' @export
StatSubset <- ggplot2::ggproto("StatSubset", ggplot2::Stat,
  required_aes = c("x", "y", "subset"),
  compute_group = function(data, scales, width = NULL, height = NULL) {
      data$width <- data$width %||% width %||% resolution(data$x, FALSE)
      data$height <- data$height %||% height %||% resolution(data$y, FALSE)
      data <- subset(data, subset == TRUE)
      data
  }
)
