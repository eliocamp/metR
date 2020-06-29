
theme_field <- function(base_size = 11, base_family = "",
                        base_line_size = base_size/22, base_rect_size = base_size/22,
                        legend.position = "bottom") {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) +
        ggplot2::theme(
        # text = element_text(family = font_rc),
        legend.position = "bottom",
        legend.box = "vertical",
        panel.spacing.y = grid::unit(5, "mm"),
        panel.spacing.x = grid::unit(5, "mm"),
        legend.spacing = grid::unit(2, "mm"),
        plot.margin = grid::unit(rep(3, 4), "mm"),
        # legend.title = element_blank(),
        legend.box.spacing = grid::unit(3, "mm"),
        legend.margin = ggplot2::margin(t = -5),
        panel.grid = ggplot2::element_line(color = "gray50", size = 0.2, linetype = 3),
        panel.ontop = TRUE)
}

