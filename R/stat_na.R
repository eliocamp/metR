#' Filter only NA values.
#'
#' Usefull for indicating or masking missing data. Ths stat subsets data where
#' one variable is `NA`.
#'
#' @inheritParams ggplot2::stat_identity
#'
#' @section Aesthetics:
#' `stat_na` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **na**
#' \item `width`
#' \item `height`
#' }
#'
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' surface <- within(surface, value[Var1 %b% c(20, 30) & Var2 %b% c(20, 30)] <- NA)
#' surface[sample(1:nrow(surface), 100, replace = FALSE), 3] <- NA
#'
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'     geom_contour_fill() +
#'     stat_na(aes(na = value))
#'
#' @seealso [stat_filter] for a more general way of filtering data.
#' @export
#' @family ggplot2 helpers
stat_na <- function(mapping = NULL, data = NULL,
                              geom = "tile", position = "identity",
                              ...,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatNa,
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
StatNa <- ggplot2::ggproto("StatNa", ggplot2::Stat,
    required_aes = c("x", "y", "na"),
    # default_aes = ggplot2::aes(fill = ..int.level..),
    compute_layer = function(self, data, params, layout) {
        ggplot2:::check_required_aesthetics(self$required_aes,
                                            c(names(data), names(params)),
                                            ggplot2:::snake_class(self))
        # Trim off extra parameters
        params <- params[intersect(names(params), self$parameters())]

        args <- c(list(data = quote(data), scales = quote(scales)), params)
        plyr::ddply(data, "PANEL", function(data) {
            scales <- layout$get_scales(data$PANEL[1])
            tryCatch(do.call(self$compute_panel, args), error = function(e) {
                warning("Computation failed in `", ggplot2:::snake_class(self), "()`:\n",
                        e$message, call. = FALSE)
                data.frame()})
            })
        },
    compute_group = function(data, scales, width = NULL, height = NULL) {
        data$width <- data$width %||% width %||% resolution(data$x, FALSE)
        data$height <- data$height %||% height %||% resolution(data$y, FALSE)

        data <- data[!(is.na(data$x) | is.na(data$y)), ]
        data <- data[is.na(data$na), ]

        data
    }
)
