#' @rdname geom_text_contour
#' @export
geom_label_contour <- function(mapping = NULL, data = NULL,
                       stat = "text_contour", position = "identity",
                       ...,
                       min.size = 5,
                       skip = 1,
                       label.placer = label_placer_flattest(),
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       label.padding = grid::unit(0.25, "lines"),
                       label.r = grid::unit(0.15, "lines"),
                       label.size = 0.25,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stopf("Specify either 'position' or 'nudge_x'/'nudge_y'",
                 call. = FALSE)
        }

        position <- ggplot2::position_nudge(nudge_x, nudge_y)
    }

    if (!is.null(list(...)$label.placement)) {
        warningf("The 'label.placement' argument is now 'label.placer'.")
        label.placer <- list(...)$label.placement
    }

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomLabelContour,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            min.size = min.size,
            skip = skip,
            parse = parse,
            label.padding = label.padding,
            label.r = label.r,
            label.size = label.size,
            label.placer = label.placer,
            na.rm = na.rm,
            ...
        )
    )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
GeomLabelContour <- ggplot2::ggproto("GeomLabelContour", ggplot2::Geom,
    required_aes = c("x", "y", "label"),
    default_aes = ggplot2::aes(
        colour = "black", fill = "white", size = 3.88, angle = 0,
        hjust = 0.5, vjust = 0.5, alpha = NA, family = "", fontface = 1,
        lineheight = 1.2
    ),

    draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                          na.rm = FALSE,
                          label.padding = unit(0.25, "lines"),
                          label.r = unit(0.15, "lines"),
                          label.size = 0.25, min.size = 20,
                          skip = 1, gap = 0, label.placer = label_placemer_flattest()) {


        data <- data.table::as.data.table(coord$transform(data, panel_params))
        min.size <- ceiling(min.size)
        if (min.size %% 2 == 0) {
            min.size <- min.size - 1
        }

        breaks <- unique(data$level)
        breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]

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
            type = "label",
            group = interaction(data$group, data$piece),
            default.units = "native",
            hjust = hjust, vjust = vjust,
            bg.r = 0, bg.color = "black",
            position = label.placer,

            col = scales::alpha(data$colour, data$alpha),
            fontsize = data$size * .pt,
            fontfamily = data$family,
            fontface = data$fontface,
            lineheight = data$lineheight,

            alpha = data$alpha,
            fill = data$fill,
            label.padding = label.padding,
            label.r = label.r,
            label.size = label.size,

            check.overlap = FALSE,
            rotate = FALSE
        )

    },

    draw_key = ggplot2::draw_key_label
)
