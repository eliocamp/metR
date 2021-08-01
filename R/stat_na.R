#' Filter only NA values.
#'
#' Useful for indicating or masking missing data. This stat subsets data where
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
#' library(data.table)
#' surface <- reshape2::melt(volcano)
#' surface <- within(surface, value[Var1 %between% c(20, 30) & Var2 %between% c(20, 30)] <- NA)
#' surface[sample(1:nrow(surface), 100, replace = FALSE), 3] <- NA
#'
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'     geom_contour_fill(na.fill = TRUE) +
#'     stat_na(aes(na = value), geom = "tile")
#'
#' @seealso [stat_subset] for a more general way of filtering data.
#' @export
#' @family ggplot2 helpers
stat_na <- function(mapping = NULL, data = NULL,
                              geom = "point", position = "identity",
                              ...,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    ggplot2::layer(
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

#' @rdname stat_na
#' @usage NULL
#' @format NULL
#' @export
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
                warningf("Computation failed in `%s()`:\n %s.",
                                 ggplot2:::snake_class(self), e$message,
                        call. = FALSE)
                data.frame()})
            })
        },
    compute_group = function(data, scales, width = NULL, height = NULL) {
        data$width <- data$width %||% width %||% ggplot2::resolution(data$x, FALSE)
        data$height <- data$height %||% height %||% ggplot2::resolution(data$y, FALSE)

        data <- data[!(is.na(data$x) | is.na(data$y)), ]
        data <- data[is.na(data$na), ]

        data
    }
)
