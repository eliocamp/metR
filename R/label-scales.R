#' Scales for contour label aesthetics
#'
#' @param aesthetics Character string or vector of character strings listing the
#' name(s) of the aesthetic(s) that this scale works with. This can be useful,
#' for example, to apply colour settings to the colour and fill aesthetics at
#' the same time, via aesthetics = c("colour", "fill").
#' @param guide Type of legend. Use "colourbar" for continuous colour bar,
#'  or "legend" for discrete colour legend.
#'
#' @export
#' @rdname label_scales
scale_label_colour_continuous <- function(..., aesthetics = c("label_colour"),
                                          guide = ggplot2::guide_colorbar(available_aes = "label_colour")) {
    ggplot2::scale_color_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @param range Output range of alpha values. Must lie between 0 and 1.
#' @export
#' @rdname label_scales
scale_label_alpha_continuous <- function(..., range = c(0.1, 1), aesthetics = c("label_alpha")) {
    ggplot2::continuous_scale("label_alpha", "label_alpha", scales::rescale_pal(range),
                              ...)
}

#' @inheritParams ggplot2::scale_size
#' @export
#' @rdname label_scales
scale_label_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                        limits = NULL, range = c(1, 6), transform = "identity",
                                        guide = "legend") {
    ggplot2::continuous_scale("label_size", "label_size", scales::area_pal(range), name = name,
                              breaks = breaks, labels = labels, limits = limits, transform = transform)
}
