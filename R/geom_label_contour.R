#' @rdname geom_text_contour
#' @export
geom_label_contour <- function(mapping = NULL, data = NULL,
                       stat = "text_contour", position = "identity",
                       ...,
                       min.size = 5,
                       skip = 0,
                       parse = FALSE,
                       nudge_x = 0,
                       nudge_y = 0,
                       label.padding = unit(0.25, "lines"),
                       label.r = unit(0.15, "lines"),
                       label.size = 0.25,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`",
                 call. = FALSE)
        }

        position <- position_nudge(nudge_x, nudge_y)
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
                          skip = 1, gap = 0) {
        data <- data.table::as.data.table(coord$transform(data, panel_params))
        min.size <- ceiling(min.size)
        if (min.size %% 2 == 0) {
            min.size <- min.size - 1
        }
        # Get points of labels
        data <- .label.position(copy(data), min.size, skip, rotate = FALSE)

        lab <- data$label
        if (parse) {
            lab <- parse(text = as.character(lab))
        }

        if (is.character(data$vjust)) {
            data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
        }
        if (is.character(data$hjust)) {
            data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
        }

        grobs <- lapply(seq_len(nrow(data)), function(i) {
            row <- data[i, , drop = FALSE]
            ggplot2:::labelGrob(lab[i],
                      x = unit(row$x, "native"),
                      y = unit(row$y, "native"),
                      just = c(row$hjust, row$vjust),
                      padding = label.padding,
                      r = label.r,
                      text.gp = grid::gpar(
                          col = row$colour,
                          fontsize = row$size * .pt,
                          fontfamily = row$family,
                          fontface = row$fontface,
                          lineheight = row$lineheight
                      ),
                      rect.gp = grid::gpar(
                          col = row$colour,
                          fill = alpha(row$fill, row$alpha),
                          lwd = label.size * .pt
                      )
            )
        })
        class(grobs) <- "gList"

        ggplot2:::ggname("geom_label_contour", grid::grobTree(children = grobs))
    },

    draw_key = ggplot2::draw_key_label
)
