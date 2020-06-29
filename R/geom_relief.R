#' Relief Shading
#'
#' `geom_relief()` simulates shading caused by relief. Can be useful when
#' plotting topographic data because relief shading might give a more intuitive
#' impression of the shape of the terrain than contour lines or mapping height
#' to colour. `geom_shadow()` projects shadows.
#'
#' @inheritParams ggplot2::geom_tile
#' @param raster if `TRUE` (the default), uses [ggplot2::geom_raster],
#' if `FALSE`, uses [ggplot2::geom_tile].
#' @param shadow if TRUE, adds also a layer of `geom_shadow()`
#' @param sun.angle angle from which the sun is shining, in degrees
#' counterclockwise from 12 o' clock
#' @param skip data points to skip when casting shadows
#' @param range transparency range for shadows
#'
#' @details
#' `light` and `dark` must be valid colours determining the light and dark shading
#'  (defaults to "white" and "gray20", respectively).
#'
#'@section Aesthetics:
#' `geom_relief()` and `geom_shadow()` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **z**
#' \item \code{light}
#' \item \code{dark}
#' \item \code{sun.angle}
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'       geom_relief(aes(z = value))
#' }
#' @family ggplot2 helpers
#'
#' @export
geom_relief <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        sun.angle = 60,
                        raster = TRUE,
                        interpolate = TRUE,
                        shadow = FALSE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

    if (shadow == TRUE){
        list(ggplot2::layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomRelief,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                raster = raster,
                interpolate = interpolate,
                na.rm = na.rm,
                sun.angle = sun.angle,
                ...
            )
        ),
        ggplot2::layer(
            data = data,
            mapping = mapping,
            stat = stat,
            geom = GeomShadow,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
                raster = raster,
                interpolate = interpolate,
                na.rm = na.rm,
                sun.angle = sun.angle,
                ...
            )
        )
    )
    } else {

    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRelief,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            raster = raster,
            interpolate = interpolate,
            na.rm = na.rm,
            sun.angle = sun.angle,
            ...
        )
    )
    }
}

#' @rdname geom_relief
#' @usage NULL
#' @format NULL
#' @export
GeomRelief <- ggplot2::ggproto("GeomRelief", ggplot2::GeomTile,
    required_aes = c("x", "y", "z"),
    default_aes = ggplot2::aes(color = NA, fill = "grey35", size = 0.5, linetype = 1,
                      alpha = NA, light = "white", dark = "gray20", sun.angle = 60),
    draw_panel = function(data, panel_scales, coord, raster, interpolate, sun.angle = 60,
                          sun.altitude = NULL, skip = NULL, alpha.range = NULL) {

        if (!coord$is_linear()) {
           stop("non lineal coordinates are not implemented in GeomRelief",
                call. = FALSE)
        } else {
            coords <- data.table::as.data.table(coord$transform(data, panel_scales))

            coords[, sun.angle := (sun.angle + 90)*pi/180]
            coords[, dx := .derv(z, x, fill = TRUE), by = y]
            coords[, dy := .derv(z, y, fill = TRUE), by = x]
            coords <- coords[!is.na(dx) & !is.na(dy)]

            coords[, shade := (cos(atan2(-dy, -dx) - sun.angle) + 1)/2]
            coords[is.na(shade), shade := 0]

            # shade.fun <- scales::colour_ramp(c(dark, light))
            # coords$fill <- shade.fun(coords$shade)
            coords[, fill := scales::colour_ramp(c(dark, light))(shade),
                   by = .(light, dark)]

            if (raster == TRUE){
                if (!inherits(coord, "CoordCartesian")) {
                    stop("geom_raster only works with Cartesian coordinates", call. = FALSE)
                }
                # Convert vector of data to raster
                x_pos <- as.integer((coords$x - min(coords$x)) / ggplot2::resolution(coords$x, FALSE))
                y_pos <- as.integer((coords$y - min(coords$y)) / ggplot2::resolution(coords$y, FALSE))

                nrow <- max(y_pos) + 1
                ncol <- max(x_pos) + 1

                raster <- matrix(NA_character_, nrow = nrow, ncol = ncol)
                raster[cbind(nrow - y_pos, x_pos + 1)] <- alpha(coords$fill, coords$alpha)

                # Figure out dimensions of raster on plot
                x_rng <- c(min(coords$xmin, na.rm = TRUE),
                           max(coords$xmax, na.rm = TRUE))
                y_rng <- c(min(coords$ymin, na.rm = TRUE),
                           max(coords$ymax, na.rm = TRUE))

                grid::rasterGrob(raster,
                           x = mean(x_rng), y = mean(y_rng),
                           width = diff(x_rng), height = diff(y_rng),
                           default.units = "native", interpolate = interpolate
                )

            } else {
                ggplot2:::ggname("geom_rect", grid::rectGrob(
                    coords$xmin, coords$ymax,
                    width = coords$xmax - coords$xmin,
                    height = coords$ymax - coords$ymin,
                    default.units = "native",
                    just = c("left", "top"),
                    gp = grid::gpar(
                        col = coords$fill,
                        fill = alpha(coords$fill, coords$alpha),
                        lwd = coords$size * .pt,
                        lty = coords$linetype,
                        lineend = "butt"
                    )
                ))

            }
        }
    }
)


# rect_to_poly <- function(xmin, xmax, ymin, ymax) {
#     data.frame(
#         y = c(ymax, ymax, ymin, ymin, ymax),
#         x = c(xmin, xmax, xmax, xmin, xmin)
#     )
# }
