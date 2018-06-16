# circular_scale <- function(aesthetics, scale_name, palette, name = waiver(),
#                              breaks = waiver(), minor_breaks = waiver(),
#                              labels = waiver(), limits = NULL,
#                              rescaler = rescale, oob = censor, expand = waiver(),
#                              na.value = NA_real_, trans = "identity",
#                              guide = "legend", position = "left",
#                              super = ScaleContinuous) {
#
#     ggplot2:::check_breaks_labels(breaks, labels)
#
#     position <- match.arg(position, c("left", "right", "top", "bottom"))
#
#     if (is.null(breaks) && !ggplot2:::is_position_aes(aesthetics) && guide != "none") {
#         guide <- "none"
#     }
#
#     trans <- as.trans(trans)
#     if (!is.null(limits)) {
#         limits <- trans$transform(limits)
#     }
#
#     ggproto(NULL, super,
#             call = match.call(),
#
#             aesthetics = aesthetics,
#             scale_name = scale_name,
#             palette = palette,
#
#             transform_df = function(self, df) {
#                 print("b")
#                 df },
#
#             range = ggplot2:::continuous_range(),
#             limits = limits,
#             trans = trans,
#             na.value = na.value,
#             expand = expand,
#             rescaler = rescaler,  # Used by diverging and n colour gradients
#             oob = oob,
#
#             name = name,
#             breaks = breaks,
#             minor_breaks = minor_breaks,
#
#             labels = labels,
#             guide = guide,
#             position = position
#     )
# }
#
# scale_x_circular <- function(name = waiver(), breaks = waiver(),
#                                minor_breaks = waiver(), labels = waiver(),
#                                limits = NULL, expand = waiver(), oob = not_censor,
#                                na.value = NA_real_, trans = "identity",
#                                position = "bottom", sec.axis = waiver()) {
#     sc <- circular_scale(
#         c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
#         "position_c", identity, name = name, breaks = breaks,
#         minor_breaks = minor_breaks, labels = labels, limits = limits,
#         expand = expand, oob = oob, na.value = na.value, trans = trans,
#         guide = "none", position = position, super = ScaleContinuousPosition
#     )
#     if (!is.waive(sec.axis)) {
#         if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
#         if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
#         sc$secondary.axis <- sec.axis
#     }
#     sc
# }
#
# not_censor <- function(x, range) {
#     x
# }
