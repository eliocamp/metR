
theme_field <- function(base_size = 11, base_family = "",
                        base_line_size = base_size/22, base_rect_size = base_size/22,
                        legend.position = "bottom") {
    theme_minimal(base_size = base_size, base_family = base_family,
                  base_line_size = base_line_size, base_rect_size = base_rect_size) +
    theme(
        # text = element_text(family = font_rc),
        legend.position = "bottom",
        legend.box = "vertical",
        panel.spacing.y = unit(5, "mm"),
        panel.spacing.x = unit(5, "mm"),
        legend.spacing = unit(2, "mm"),
        plot.margin = grid::unit(rep(3, 4), "mm"),
        # legend.title = element_blank(),
        legend.box.spacing = unit(3, "mm"),
        legend.margin = margin(t = -5),
        panel.grid = element_line(color = "gray50", size = 0.2, linetype = 3),
        panel.ontop = TRUE)
}

