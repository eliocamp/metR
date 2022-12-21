#
# stat_bin_circular <- function(mapping = NULL, data = NULL,
#                      geom = "bar", position = "stack",
#                      ...,
#                      range = NULL,
#                      binwidth = NULL,
#                      bins = NULL,
#                      center = NULL,
#                      boundary = NULL,
#                      breaks = NULL,
#                      closed = c("right", "left"),
#                      pad = FALSE,
#                      na.rm = FALSE,
#                      show.legend = NA,
#                      inherit.aes = TRUE) {
#
#     ggplot2::layer(
#         data = data,
#         mapping = mapping,
#         stat = StatBinCircular,
#         geom = geom,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(
#             range = range,
#             binwidth = binwidth,
#             bins = bins,
#             center = center,
#             boundary = boundary,
#             breaks = breaks,
#             closed = closed,
#             pad = pad,
#             na.rm = na.rm,
#             ...
#         )
#     )
# }
#
# StatBinCircular <- ggplot2::ggproto("StatBinCircular", StatBin,
#   setup_data = function(data, params) {
#       if (is.null(params$range)) params$range <- range(data$x)
#       d <- diff(params$range)
#
#       data1 <- data
#       data1$x <- data1$x - d
#
#       data2 <- data
#       data2$x <- data2$x + d
#       rbind(data1, data, data2)
#   },
#   compute_group = function(data, scales, binwidth = NULL, bins = NULL,
#                            center = NULL, boundary = NULL,
#                            closed = c("right", "left"), pad = FALSE,
#                            # The following arguments are not used, but must
#                            # be listed so parameters are computed correctly
#                            breaks = NULL, origin = NULL, right = NULL,
#                            drop = NULL, width = NULL, range = NULL) {
#
#       StatBin$compute_group(data, scales, binwidth = binwidth, bins = bins,
#                             center = center, boundary = boundary,
#                             closed = closed, pad = pad,
#                             # The following arguments are not used, but must
#                             # be listed so parameters are computed correctly
#                             breaks = breaks, origin = origin, right = right,
#                             drop = drop, width = width)
#   })
#
#
#
#
# # w <- data.frame(u = rnorm(1000, 4, 2),
# #                 v = rnorm(1000, -0.5, 3))
# # w$dir <- with(w, ConvertLongitude(atan2(v, u)*180/pi, from = 180))
# # # w$dir <- runif(100, 0, 360)
# #
# # ggplot(w, aes(dir, fill = cut(sqrt(u^2 + v^2), 2))) +
# #     # stat_bin(breaks = seq(0, 360, by = 22.5) - 22.5/2) +
# #     stat_bin_circular(breaks = seq(0, 360, by = 22.5) - 22.5/2, range = c(0, 360)) +
# #     # geom_rug() +
# #     scale_x_continuous(breaks = seq(0, 360, by = 22.5)) +
# #     coord_polar(start = -22.5/2*pi/180)
#
#
#
# library(ggplot2)
# library(data.table)
#
# # Datos de este link: http://midcdmz.nrel.gov/apps/plot.pl?site=NWTC&start=20010824&edy=26&emo=3&eyr=2062&year=2013&month=1&day=1&endyear=2013&endmonth=12&endday=31&time=0&inst=21&inst=39&type=data&wrlevel=2&preset=0&first=3&math=0&second=-1&value=0.0&user=0&axis=1
# wind <- fread("~/Downloads/20130101.csv")
#
# # La función cut me da los intervalos en orden decresciente, pero
# # los necesito en orden cresciente.
# ReverseCut <- function(x, ...) {
#     f <- cut(x, ...)
#     n <- length(levels(f))
#     levels(f) <- levels(f)[n:1]
#     f
# }
#
# # Truquito: mover el dominio de la dirección a (-22.5/2, 360 - 22.5/2).
# wind[, new.dir := ifelse(direction > 360 - 22.5/2, direction - 360, direction)]
#
# # Me quedo con 1000 al azar para que no tarde tanto en plotear.
# set.seed(1)
# wind <- wind[sample(1:.N, 1000)]
#
# ggplot(wind, aes(new.dir, fill = ReverseCut(speed, breaks = 5))) +
#     # geom_histogram(binwidth = 22.5, center = 0, aes(y = after_stat(count)/1000*100)) +
#     stat_bin_circular(binwidth = 22.5, center = 0, range = c(0, 360), aes(x = direction, y = after_stat(count)/1000*100)) +
#     coord_polar(start = -22.5/2*pi/180) +    # para que el norte quede arriba
#     scale_fill_viridis_d(name = "Velocidad", direction = -1) +
#     scale_y_continuous(name = "Frecuencia",
#                        limits = c(-0.50, NA)) +
#     annotate(geom = "rect", ymin = -0.5, ymax = 0, xmin = 0-22.5/2, xmax =360-22.5/2,
#              fill = "white") +    # círculo blanco del centro
#     scale_x_continuous(name = "", limits = c(0 - 22.5/2, 360 - 22.5/2),
#                        breaks = seq(0, 360 - 22.5, by = 22.5),
#                        minor_breaks = NULL,
#                        labels = c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
#                                   "S", "SSO", "SO", "OSO", "O", "ONO", "NO", "NNO")) +
#     # geom_text(data = data.frame(x = 0, y = seq(0, 12, by = 3)), aes(x = x, y = y, label = y),
#               # inherit.aes = F) +
#     theme_minimal()
