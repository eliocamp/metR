#' @rdname geom_relief
#' @export
geom_shadow <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        sun.angle = 60,
                        range = c(0, 1),
                        skip = 0,
                        raster = TRUE,
                        interpolate = TRUE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

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
            alpha.range = range,
            skip = skip,
            ...
        )
    )
}

#' @rdname geom_relief
#' @usage NULL
#' @format NULL
#' @export
GeomShadow <- ggplot2::ggproto("GeomShadow", ggplot2::GeomTile,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(color = NA, fill = "black", size = 0.5, linetype = 1,
                             sun.angle = 60, sun.altitude = 20),
  draw_panel = function(data, panel_scales, coord, raster, interpolate,
                        alpha.range = c(0, 1), skip = 0, alpha = NULL) {

      if (!coord$is_linear()) {
          stopf("Non lineal coordinates are not implemented in GeomShadow.",
               call. = FALSE)
      } else {

          coords <- data.table::as.data.table(coord$transform(data, panel_scales))
          coords <- coords[x %in% JumpBy(unique(x), skip + 1) &
                           y %in% JumpBy(unique(y), skip + 1)]
          alpha <- .cast_shadow(coords[, .(x, y, z, sun.angle, sun.altitude)], skip)
          alpha.range <- scales::squish(alpha.range)
          coords[, alpha := scales::rescale(alpha, to = alpha.range)]


          if (raster == TRUE){
              if (!inherits(coord, "CoordCartesian")) {
                  stopf("geom_raster only works with Cartesian coordinates.", call. = FALSE)
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


.cast_shadow <- function(coords, skip) {
    coords[, z1 := scales::rescale(z)]

    m <- data.table::dcast(coords, x ~ y, value.var = "z1")
    xdim <- m$x
    ydim <- as.numeric(colnames(m)[-1])
    m <- as.matrix(m[, -1])
    coords[, alpha := .shadow(x, y, m, xdim, ydim, sun.angle, sun.altitude),
           by = .(x, y)]
    return(coords$alpha)
}

.shadow <- function(x, y, m, xdim, ydim, azimuth, altitude) {
    rx <- range(xdim)
    ry <- range(ydim)

    a <- tan(azimuth*pi/180)
    xs <- x - (ydim - y)*a
    ys <- y - (xdim - x)/a

    xs <- c(xs, xdim)
    ys <- c(ydim, ys)

    azimuth <- azimuth %% 360
    # Cut irrelevant cuadrants
    if (azimuth >= 0 & azimuth < 90) {
        keep <- xs <= x & ys > y
    } else if (azimuth >= 90 & azimuth < 180) {
        keep <- xs < x & ys <= y
    } else if (azimuth >= 180 & azimuth < 270) {
        keep <- xs >= x & ys < y
    } else{
        keep <- xs > x & ys >= y
    }
    xs <- xs[keep]
    ys <- ys[keep]
    # Cut data outside range
    keep <- xs %between% rx & ys %between% ry
    ys <- ys[keep]
    xs <- xs[keep]

    z.interpol <- function(x, y, m) {
        interpolate_locations(list(x = xdim, y = ydim, z = m),
                               matrix(c(x, y), ncol = 2))
    }

    h <- z.interpol(c(x, xs), c(y, ys), m)
    h0 <- h[1]
    h <- h[-1]
    # h0 <- z.interpol(x, y, m)

    angle <- suppressWarnings(max((h - h0)/sqrt((xs - x)^2 + (ys - y)^2)/10, na.rm = TRUE))

    if (!is.finite(angle)){
        d <- 0
    } else if (angle < tan(altitude*pi/180)) {
        d <- 0
    } else {
        d <- sqrt(angle - tan(altitude*pi/180))
    }
    d
}
