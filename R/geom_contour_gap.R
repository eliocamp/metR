geom_contour_ <- function(...) {
    list(
        geom_contour_gap(...),
        geom_text_contour(...)
    )
}



geom_contour_gap <- function(mapping = NULL, data = NULL,
                             stat = "contour2",
                             position = "identity",
                             ...,
                             gap = 5,
                             skip = 1,
                             breaks = scales::fullseq,
                             bins = NULL,
                             binwidth = NULL,
                             circular = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE)
{

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContourGap,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            skip = skip,
            gap = gap,
            na.rm = na.rm,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            circular = circular,
            ...
        )
    )
}


GeomContourGap <- ggplot2::ggproto("GeomContourGap", GeomPath,
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, gap = 0, skip = 1) {
      if (!anyDuplicated(data$group)) {
          message_wrap("geom_path: Each group consists of only one observation. ",
                       "Do you need to adjust the group aesthetic?")
      }

      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]
      data <- data.table::as.data.table(coord_munch(coord, data, panel_params))

      breaks <- unique(data$level)
      breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]


      data[, id := 1:.N, by = piece]
      data.high <- data[, .(x = approx(id, x, n = length(x)*3)$y,
                            y = approx(id, y, n = length(y)*3)$y), by = piece]
      data <- data.high[data[, -c("x", "y")][, .SD[1], by = piece], on = "piece"]

      data[, N := .N, by = piece]
      # Cut contours with labels.
      data[, cut := level %in% breaks.cut & N > (gap + 3) ]

      # Check if point has minimum variance
      data[, var := minvar(x, y), by = .(piece)]
      data[is.na(var), var := FALSE]

      # Distance in points
      data[, point := seq_len(.N), by = piece]
      data[, d := point - point[var == TRUE][1], by = piece]


      data[cut == TRUE, group := interaction(piece, sign(d))]

      # Remove close points
      data <- data[abs(d) > gap | !cut]

      ## --- Original ggplot2 code below ----

      # Silently drop lines with less than two points, preserving order
      rows <- stats::ave(seq_len(nrow(data)), data$group, FUN = length)
      data <- data[rows >= 2, ]
      if (nrow(data) < 2) return(zeroGrob())

      # Work out whether we should use lines or segments
      attr <- plyr::ddply(data, "group", function(df) {
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

      # Work out grouping variables for grobs
      n <- nrow(data)
      group_diff <- data$group[-1] != data$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)

      if (!constant) {
          segmentsGrob(
              data$x[!end], data$y[!end], data$x[!start], data$y[!start],
              default.units = "native", arrow = arrow,
              gp = gpar(
                  col = alpha(data$colour, data$alpha)[!end],
                  fill = alpha(data$colour, data$alpha)[!end],
                  lwd = data$size[!end] * .pt,
                  lty = data$linetype[!end],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      } else {
          id <- match(data$group, unique(data$group))
          polylineGrob(
              data$x, data$y, id = id,
              default.units = "native", arrow = arrow,
              gp = gpar(
                  col = alpha(data$colour, data$alpha)[start],
                  fill = alpha(data$colour, data$alpha)[start],
                  lwd = data$size[start] * .pt,
                  lty = data$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      }
  })


