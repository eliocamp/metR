#' Label contours
#'
#' Draws labels on contours built with [ggplot2::stat_contour].
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_label
#' @inheritParams geom_contour2
#' @param min.size minimum number of points for a contour to be labelled.
#' @param skip number of contours to skip
#' @param rotate logical indicating whether to rotate text following the contour.
#' @param stroke numerical indicating width of stroke relative to the size of
#' the text. Ignored if less than zero.
#' @param stroke.color any valid colour.
#' @param label.placement A function for placing labels (see `label_placement_flattest()`).
#'
#' @details
#' Is best used with a previous call to [ggplot2::stat_contour] with the same
#' parameters (e.g. the same `binwidth`, `breaks`, or `bins`).
#' Note that while `geom_text_contour()` can angle itself to follow the contour,
#' this is not the case with `geom_label_contour()`.
#'
#' @examples
#' library(ggplot2)
#' v <- reshape2::melt(volcano)
#' g <- ggplot(v, aes(Var1, Var2)) +
#'        geom_contour(aes(z = value))
#' g + geom_text_contour(aes(z = value))
#'
#' g + geom_text_contour(aes(z = value), stroke = 0.2)
#'
#' g + geom_text_contour(aes(z = value), rotate = FALSE)
#'
#' g + geom_text_contour(aes(z = value),
#'                       label.placement = label_placement_random())
#'
#' g + geom_text_contour(aes(z = value),
#'                       label.placement = label_placement_n(3))
#'
#'
#' g + geom_text_contour(aes(z = value),
#'                       label.placement = label_placement_flattest())
#'
#' g + geom_text_contour(aes(z = value),
#'                       label.placement = label_placement_flattest(ref_angle = 90))
#'
#' @section Aesthetics:
#' \code{geom_text_contour} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{label}
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
#' @family ggplot2 helpers
#' @importFrom ggplot2 .pt
geom_text_contour <- function(mapping = NULL, data = NULL,
                      stat = "text_contour",
                      position = "identity",
                      ...,
                      min.size = 5,
                      skip = 1,
                      rotate = TRUE,
                      label.placement = label_placement_flattest(),
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      stroke = 0,
                      stroke.color = "white",
                      check_overlap = FALSE,
                      # xwrap = NULL,
                      # ywrap = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`",
                 call. = FALSE)
        }

        position <- ggplot2::position_nudge(nudge_x, nudge_y)
    }

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomTextContour,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            skip = skip,
            min.size = min.size,
            rotate = rotate,
            parse = parse,
            check_overlap = check_overlap,
            stroke = stroke,
            stroke.color = stroke.color,
            # xwrap = xwrap,
            # ywrap = ywrap,
            label.placement = label.placement,
            na.rm = na.rm,
            ...
        )
    )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
# #' @importFrom shadowtext shadowtextGrob
GeomTextContour <- ggplot2::ggproto("GeomTextContour", ggplot2::Geom,
   required_aes = c("x", "y", "label"),
   default_aes = ggplot2::aes(colour = "black", size = 3.88, angle = 0,
                              hjust = 0.5, vjust = 0.5, alpha = NA, family = "",
                              fontface = 1, lineheight = 1.2),

   draw_panel = function(data, panel_params, coord, parse = FALSE,
                         na.rm = FALSE, check_overlap = FALSE, min.size = 20,
                         skip = 1, rotate = FALSE, gap = NULL,
                         label.placement = label_placement_flattest(),
                         stroke = 0, stroke.colour = "white") {
       data <- data.table::as.data.table(coord$transform(data, panel_params))
       min.size <- ceiling(min.size)
       if (min.size %% 2 == 0) {
           min.size <- min.size - 1
       }

       breaks <- unique(data$level)
       breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]

       # browser()
       data <- data[level %in% breaks.cut]

       data <- data[, unique(.SD, by = c("x", "y")),by = .(group, piece)]

       data[, N := .N, by = .(group, piece)]
       data <- data[N > 3]
       data <- data[N >= min.size][, N := NULL]

       if (nrow(data) == 0) {
           return(grid::nullGrob())
       }

       ## Original ggplot2 here.
       lab <- data$label
       if (parse) {
           lab <- parse(text = as.character(lab))
       }

       if (is.character(data$vjust)) {
           vjust <- ggplot2:::compute_just(data$vjust, data$y)[1]
       } else {
           vjust <- data$vjust[1]
       }
       if (is.character(data$hjust)) {
           hjust <- ggplot2:::compute_just(data$hjust, data$x)[1]
       } else {
           hjust <- data$hjust[1]
       }

       contourTextGrob(
           lab,
           data$x, data$y,
           group = interaction(data$group, data$piece),
           default.units = "native",
           hjust = hjust, vjust = vjust,
           bg.r = stroke, bg.color = stroke.colour,
           position = label.placement,

               col = scales::alpha(data$colour, data$alpha),
               fontsize = data$size * .pt,
               fontfamily = data$family,
               fontface = data$fontface,
               lineheight = data$lineheight,

           check.overlap = check_overlap,
           rotate = rotate
       )

   },

   draw_key = ggplot2::draw_key_text
)


