#' Illuminated contours
#'
#' Illuminated contours (aka Tanaka contours) use varying brightness and width to
#' create an illusion of relief. This can help distinguishing between concave and
#' convex areas (local minimums and maximums), specially in black and white plots
#' or to make photocopy safe plots with divergent colour palettes, or to render
#' a more aesthetically pleasing representation of topography.
#'
#' @inheritParams geom_contour2
#' @inheritParams geom_relief
#' @param sun.angle angle of the sun in degrees counterclockwise from 12 o' clock
#' @param light,dark valid colour representing the light and dark shading
#' @param range numeric vector of length 2 with the minimum and maximum size of lines
#' @param smooth numeric indicating the degree of smoothing of illumination and size.
#' Larger
#'
#' @section Aesthetics:
#' `geom_contour_tanaka` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **z**
#' \item \code{linetype}
#' }
#'
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#'
#' library(ggplot2)
#' library(data.table)
#' # A fresh look at the boring old volcano dataset
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour_fill(aes(z = value)) +
#'     geom_contour_tanaka(aes(z = value)) +
#'     theme_void()
#'
#' # If the transition between segments feels too abrupt,
#' # smooth it a bit with smooth
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour_fill(aes(z = value)) +
#'     geom_contour_tanaka(aes(z = value), smooth = 1) +
#'     theme_void()
#'
#' data(geopotential)
#' geo <- geopotential[date == unique(date)[4]]
#' geo[, gh.z := Anomaly(gh), by = lat]
#'
#' # In a monochrome contour map, it's impossible to know which areas are
#' # local maximums or minimums.
#' ggplot(geo, aes(lon, lat)) +
#'     geom_contour2(aes(z = gh.z), color = "black", xwrap = c(0, 360))
#'
#' # With tanaka contours, they are obvious.
#' ggplot(geo, aes(lon, lat)) +
#'     geom_contour_tanaka(aes(z = gh.z), dark = "black",
#'                         xwrap = c(0, 360)) +
#'     scale_fill_divergent()
#'
#' # A good divergent color palette has the same luminosity for positive
#' # and negative values.But that means that printed in grayscale (Desaturated),
#' # they are indistinguishable.
#' (g <- ggplot(geo, aes(lon, lat)) +
#'     geom_contour_fill(aes(z = gh.z), xwrap = c(0, 360)) +
#'     scale_fill_gradientn(colours = c("#767676", "white", "#484848"),
#'                          values = c(0, 0.415, 1)))
#'
#' # Tanaka contours can solve this issue.
#' g + geom_contour_tanaka(aes(z = gh.z))
#'
#' @export
geom_contour_tanaka <- function(mapping = NULL, data = NULL,
                                stat = "Contour2", position = "identity",
                                ...,
                                breaks = NULL,
                                bins = NULL,
                                binwidth = NULL,
                                sun.angle = 60,
                                light = "white",
                                dark = "gray20",
                                range = c(0.01, 0.5),
                                smooth = 0,
                                proj = NULL,
                                clip = NULL,
                                kriging = FALSE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
    .check_wrap_param(list(...))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContourTanaka,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            na.rm = na.rm,
            sun.angle = sun.angle,
            light = light,
            dark = dark,
            range = range,
            smooth = smooth,
            kriging = kriging,
            proj = proj,
            clip = clip,
            ...
        )
    )
}

#' @rdname geom_contour_tanaka
#' @usage NULL
#' @format NULL
#' @export
GeomContourTanaka <- ggplot2::ggproto("GeomContourTanaka", ggplot2::GeomPath,
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        na.rm = FALSE, sun.angle = 60, light = "gray20", dark = "black",
                        range = c(0.01, 0.5), smooth = 0) {
      if (!anyDuplicated(data$group)) {
        messagef("%s: Each group consists of only one observation.\nDo you need to adjust the group aesthetic?", "geom_path")
      }

      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]

      data.table::setDT(data)

      data[, dx := c(diff(x), 0), by = group]
      data[, dy := c(diff(y), 0), by = group]

      munched <- ggplot2::coord_munch(coord, data, panel_params)

      # Silently drop lines with less than two points, preserving order
      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
      munched <- munched[rows >= 2, ]
      if (nrow(munched) < 2) return(ggplot2::zeroGrob())

      munched <- data.table::setDT(munched)
      munched[, dx := c(diff(x), NA), by = group]
      munched[, dy := c(diff(y), NA), by = group]
      # Remove duplicated points.
      munched <- munched[is.na(dx + dy) | !(dx == 0 & dy == 0)]
      munched <- munched[, .SD[.N >=  2], by = group]

      munched[, sun.angle := (sun.angle + 90)*pi/180]
      munched[, relative.angle := sun.angle - atan2(dy, dx)]
      munched[, relative.angle := c(relative.angle[-.N], relative.angle[.N]), by = group]
      munched[, shade := (sin(relative.angle))]


      if (smooth > 0) {
        smooth <- 2 + floor(2*smooth)

        munched[is.na(alpha), alpha := 1]
        munched <- munched[, c(supersample_path(x, y, smooth),
                               list(shade = approx(seq_len(.N), shade, method = "constant",
                                                   n = 1 + (.N-1)*smooth)[["y"]],
                                    alpha = approx(seq_len(.N),  method = "constant",
                                                   alpha, n =  1 + (.N-1)*smooth)[["y"]],
                                    linetype = approx(seq_len(.N), linetype, method = "constant",
                                                      n =  1 + (.N-1)*smooth)[["y"]])),
                           by = .(group)]

        smooth <- max(3, smooth - 1)
        d <- seq(1, smooth)
        d <- d - d[ceiling(smooth/2)]
        d <- d/(max(d)+1)

        w <- (1 - abs(d)^3)^3
        w <- w/sum(w)

        munched[, shade := as.vector(stats::filter(shade, w, circular = TRUE)),
                by = group]
      }


      munched[, size := abs(shade)]
      munched[, shade := scales::rescale(shade, c(0, 1), c(-1, 1))]

      munched[, dark := dark][, light := light]

      munched[!is.na(shade),
              colour :=  scales::colour_ramp(c(dark, light))(shade),
              by = .(dark, light)]
      munched[, size := scales::rescale(size, to = range)]

      # Work out whether we should use lines or segments
      attr <- plyr::ddply(munched, "group", function(df) {
        linetype <- unique(df$linetype)
        data.frame(
              solid = identical(linetype, 1) || identical(linetype, "solid"),
              constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
          )
      })
      solid_lines <- all(attr$solid)
      constant <- all(attr$constant)
      if (!solid_lines && !constant) {
        stopf("%s: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.",
             "geom_path", call. = FALSE)
      }

      # munched[delta > 0, fill := .rgb2
      # Work out grouping variables for grobs
      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)

      if (!constant) {
          grid::segmentsGrob(
              munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
              default.units = "native", arrow = arrow,
              gp = grid::gpar(
                  col = scales::alpha(munched$colour, munched$alpha)[!end],
                  fill = scales::alpha(munched$colour, munched$alpha)[!end],
                  lwd = munched$size[!end] * .pt,
                  lty = munched$linetype[!end],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      } else {
          id <- match(munched$group, unique(munched$group))
          grid::polylineGrob(
              munched$x, munched$y, id = id,
              default.units = "native", arrow = arrow,
              gp = grid::gpar(
                  col = scales::alpha(munched$colour, munched$alpha)[start],
                  fill = scales::alpha(munched$colour, munched$alpha)[start],
                  lwd = munched$size[start] * .pt,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      }
  }
)

.addpiece <- function(remove, piece) {
    cuts <- which(remove)
    if (length(remove) > max(cuts)) cuts <- c(cuts, length(remove)+1)
    if (min(cuts) > 1) cuts <- c(1, cuts)
    piece <- unlist(lapply(seq_along(cuts[-1]), function(x) {
        piece[cuts[x]:(cuts[x+1]-1)] <- piece[cuts[x]:(cuts[x+1]-1)] + x - 1
    }))
}

supersample_path <- function(x, y, n) {
  dx <- diff(x)
  dy <- diff(y)
  new_x <- c(x[1], x[1] + cumsum(rep(dx, each = n)/n))
  new_y <- c(y[1], y[1] + cumsum(rep(dy, each = n)/n))
  return(list(x = new_x, y = new_y))
}
