#' `light` and `dark` must be valid colours determining the light and dark shading
#'  (defaults to "white" and "gray20", respectively). `sun.angle` is the angle,
#'  in degrees counterclockwise from 12 o' clock, from which the sun is shining
#'  (defaults to 60).

GeomContourTanaka <- ggplot2::ggproto("GeomContourTanaka", GeomPath,
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        na.rm = FALSE, sun.angle = 60, light = "gray20", dark = "black",
                        range = c(0.01, 0.5)) {
      if (!anyDuplicated(data$group)) {
          message_wrap("geom_path: Each group consists of only one observation. ",
                       "Do you need to adjust the group aesthetic?")
      }

      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]

      rx <- ggplot2::resolution(data$x, zero = FALSE)
      ry <- ggplot2::resolution(data$y, zero = FALSE)
      setDT(data)

      data[, dx := c(diff(x), 0), by = group]
      data[, dy := c(diff(y), 0), by = group]
      # data[, remove := ifelse(dx == 0 | dx == 0, TRUE, FALSE)]
      # data[, piece2 := .addpiece(remove, piece), by = group]

      munched <- coord_munch(coord, data, panel_params)

      # munched <- subset(munched, is.na(remove))
      # Silently drop lines with less than two points, preserving order
      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
      munched <- munched[rows >= 2, ]
      if (nrow(munched) < 2) return(zeroGrob())

      munched <- setDT(munched)
      munched[, dx := c(diff(x), 0), by = group]
      munched[, dy := c(diff(y), 0), by = group]

      # munched[, delta := sign(int.level - level)]
      munched[, sun.angle := (sun.angle + 90)*pi/180]
      munched[, relative.angle := sun.angle - atan2(dy, dx)]
      munched[, shade := (sin(relative.angle))]
      munched[, shade := scales::rescale(shade, c(0, 1), c(-1, 1))]
      munched[, size := abs(sin(relative.angle))]

      munched[, dark := dark][, light := light]

      munched[!is.na(shade),
              colour := .rgb2hex(grDevices::colorRamp(c(dark, light), space = "Lab")(shade)),
              by = .(dark, light)]

      # munched[!is.na(size) & delta < 0,
      #         colour := .rgb2hex(grDevices::colorRamp(c(light, dark), space = "Lab")(shade)),
      #         by = .(dark, light)]
      munched[, size := scales::rescale(size, to = range)]

    # munched[group == group[1], c("size") := .(0)]


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
          stop("geom_path: If you are using dotted or dashed lines",
               ", colour, size and linetype must be constant over the line",
               call. = FALSE)
      }

      # munched[delta > 0, fill := .rgb2
      # Work out grouping variables for grobs
      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)

      if (!constant) {
          segmentsGrob(
              munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
              default.units = "native", arrow = arrow,
              gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[!end],
                  fill = alpha(munched$colour, munched$alpha)[!end],
                  lwd = munched$size[!end] * .pt,
                  lty = munched$linetype[!end],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      } else {
          print("constant")
          id <- match(munched$group, unique(munched$group))
          polylineGrob(
              munched$x, munched$y, id = id,
              default.units = "native", arrow = arrow,
              gp = gpar(
                  col = alpha(munched$colour, munched$alpha)[start],
                  fill = alpha(munched$colour, munched$alpha)[start],
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

geom_contour_tanaka <- function(mapping = NULL, data = NULL,
                                stat = "Contour2", position = "identity",
                                ...,
                                breaks = NULL,
                                bins = NULL,
                                binwidth = NULL,
                                sun.angle = 60,
                                light = "white",
                                dark = "gray20",
                                na.rm = FALSE,
                                circular = NULL,
                                show.legend = NA,
                                inherit.aes = TRUE) {
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
            circular = circular,
            sun.angle = sun.angle,
            light = light,
            dark = dark,
            ...
        )
    )
}





