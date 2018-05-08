#' 2d contours of a 3d surface
#'
#' A copy of [ggplot2::geom_contour] that accepts a function as the `breaks`
#' argument and makes gaps for labels
#'
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_text_contour
#' @param gap half the size of the gap in points
#'
#' @section Aesthetics:
#' \code{geom_contour2} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#'  \item \code{alpha}
#'  \item \code{colour}
#'  \item \code{group}
#'  \item \code{linetype}
#'  \item \code{size}
#'  \item \code{weight}
#'}
#'
#' @examples
#' library(ggplot2)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, color = ..level..),
#'                   breaks = AnchorBreaks(130, binwidth = 11))
#'
#' ggplot(geopotential[date == date[1]], aes(lon, lat, z = gh)) +
#'     geom_contour2(aes(color = ..level..), gap = 6) +
#'     geom_text_contour()
#'
#' # For a quick plots and to ensure that every parameter
#' # is the same for both labels and contours, define your
#' # own function
#' geom_contour_labeled <- function(..., gap = 5) {
#'     list(geom_contour2(..., gap = gap),
#'          geom_text_contour(...))
#' }
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour_labeled()
#'
#' @family ggplot2 helpers
#' @export
geom_contour2 <- function(mapping = NULL, data = NULL,
                         stat = "contour2", position = "identity",
                         ...,
                         gap = 0,
                         min.size = 5,
                         skip = 1,
                         lineend = "butt",
                         linejoin = "round",
                         linemitre = 1,
                         breaks = scales::fullseq,
                         bins = NULL,
                         binwidth = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomContour2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      breaks = breaks,
      bins = bins,
      binwidth = binwidth,
      na.rm = na.rm,
      gap = gap,
      min.size = min.size,
      skip = skip,
      ...
    )
  )
}


#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
GeomContour2 <- ggplot2::ggproto("GeomContour2", GeomPath,
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, gap = 0, skip = 1, min.size = 0,
                        rotate = FALSE) {
      if (!anyDuplicated(data$group)) {
          message_wrap("geom_path: Each group consists of only one observation. ",
                       "Do you need to adjust the group aesthetic?")
      }
      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]
      data <- data.table::as.data.table(coord_munch(coord, data, panel_params))
      if (gap > 0) {
          # Check wich contours are labeled and where.
          min.size <- ceiling(min.size)
          if (min.size %% 2 == 0) {
              min.size <- min.size - 1
          }
          labels <- .label.position(data, min.size, skip, FALSE)
          data <- labels[, .(x, y, cut = TRUE)][data, on = c("x", "y")]
          data[is.na(cut), cut := FALSE]

          # Distance in points
          data[, point := seq_len(.N), by = piece]
          data[piece %in% labels$piece, d := as.numeric(point - point[cut == TRUE][1]), by = piece]
          # data[is.na(d), d := max(d, na.rm = T), by = piece]
dd <<- copy(data)
          if (gap != round(gap)) {
              small.piece <- data[abs(d) <= ceiling(gap) & !is.na(d)]
              # Improve resolution at segments close to the gap
              # res <- ceiling(1/(gap - floor(gap)))
              # # res <- 10
              # data.high <- small.piece[, .(x = approx(point, x, n = length(x)*res)$y,
              #                              y = approx(point, y, n = length(y)*res)$y,
              #                              point = approx(point, point, n = length(y)*res)$y,
              #                              d = approx(point, d, n = length(point)*res)$y),
              #                          by = piece]

              ends <- c(-gap, gap)
              data.high <- small.piece[, .(x = approx(d, x, xout = ends)$y,
                                           y = approx(d, y, xout = ends)$y,
                                           point = approx(d, point, xout = ends)$y,
                                           d = approx(d, d, xout = ends)$y),
                                       by = piece]
              # Remove points
              # data.high <- data.high[abs(d) > gap]
              # Add other columns
              data.high <- data.high[data[abs(d) < ceiling(gap) & !is.na(d)][
                  , .(level, piece, colour, size, linetype, alpha, group)][
                      , .SD[1], by = piece], on = "piece"]

              # Merge, reorder and regroup
              data <- rbind(data[abs(d) > ceiling(gap) | is.na(d),
                                 .(x, y, level, piece, colour, size, linetype, alpha, point, d, group)],
                            data.high)
              data <- data[order(piece, point)]
              data[piece %in% labels$piece, group := interaction(piece, sign(d))]
          } else {
              # Remove close points and regroup
              data[piece %in% labels$piece, group := interaction(piece, sign(d))]
              data <- data[abs(d) > gap | !(piece %in% labels$piece)]
          }
      }

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


