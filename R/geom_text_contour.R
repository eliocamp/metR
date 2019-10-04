#' Label contours
#'
#' Draws labels on contours built with [ggplot2::stat_contour].
#'
#' @inheritParams ggplot2::geom_text
#' @inheritParams ggplot2::geom_label
#' @inheritParams geom_contour2
#' @param min.size minimum number of points for a contour to be labeled.
#' @param skip number of contours to skip
#' @param rotate logical indicating whether to rotate text following the contour.
#' @param stroke numerical indicating width of stroke relative to the size of
#' the text. Ignored if less than zero.
#' @param stroke.color any valid color.
#'
#' @details
#' Is best used with a previous call to [ggplot2::stat_contour] with the same
#' parameters.
#' Note that while `geom_text_contour()` can angle itself to follow the contour,
#' this is not the case with `geom_label_contour()`.
#'
#' @examples
#' library(ggplot2)
#' v <- reshape2::melt(volcano)
#' g <- ggplot(v, aes(Var1, Var2)) +
#'        geom_contour(aes(z = value))
#' g + geom_text_contour(aes(z = value))
#'
#' g + geom_text_contour(aes(z = value), stroke = 0.2)
#'
#' @section Aesthetics:
#' \code{geom_text_contour} understands the following aesthetics (required aesthetics are in bold):
#'
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{label}
#' \item \code{alpha}
#' \item \code{angle}
#' \item \code{colour}
#' \item \code{family}
#' \item \code{fontface}
#' \item \code{group}
#' \item \code{hjust}
#' \item \code{lineheight}
#' \item \code{size}
#' \item \code{vjust}
#'}
#'
#'
#' @export
#' @import ggplot2 data.table
#' @family ggplot2 helpers
geom_text_contour <- function(mapping = NULL, data = NULL,
                      stat = "text_contour",
                      position = "identity",
                      ...,
                      min.size = 5,
                      skip = 0,
                      rotate = TRUE,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      stroke = 0,
                      stroke.color = "white",
                      check_overlap = FALSE,
                      # xwrap = NULL,
                      # ywrap = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`",
                 call. = FALSE)
        }

        position <- position_nudge(nudge_x, nudge_y)
    }

    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomTextContour,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            skip = skip,
            min.size = min.size,
            rotate = rotate,
            parse = parse,
            check_overlap = check_overlap,
            stroke = stroke,
            stroke.color = stroke.color,
            # xwrap = xwrap,
            # ywrap = ywrap,
            na.rm = na.rm,
            ...
        )
    )
}

#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
# #' @importFrom shadowtext shadowtextGrob
GeomTextContour <- ggplot2::ggproto("GeomTextContour", ggplot2::Geom,
   required_aes = c("x", "y", "label"),
   default_aes = ggplot2::aes(colour = "black", size = 3.88, angle = 0,
                              hjust = 0.5, vjust = 0.5, alpha = NA, family = "",
                              fontface = 1, lineheight = 1.2),

   draw_panel = function(data, panel_params, coord, parse = FALSE,
                         na.rm = FALSE, check_overlap = FALSE, min.size = 20,
                         skip = 1, rotate = FALSE, gap = NULL,
                         stroke = 0, stroke.colour = "white") {
       data <- data.table::as.data.table(coord$transform(data, panel_params))
       min.size <- ceiling(min.size)
       if (min.size %% 2 == 0) {
           min.size <- min.size - 1
       }
       # Get points of labels
       data <- .label.position(copy(data), min.size, skip, rotate)
       if (nrow(data) == 0) {
           return(nullGrob())
       }

       ## Original ggplot2 here.
       lab <- data$label
       if (parse) {
           lab <- parse(text = as.character(lab))
       }

       if (is.character(data$vjust)) {
           data$vjust <- ggplot2:::compute_just(data$vjust, data$y)
       }
       if (is.character(data$hjust)) {
           data$hjust <- ggplot2:::compute_just(data$hjust, data$x)
       }

       shadowtext(
           lab,
           data$x, data$y, default.units = "native",
           hjust = data$hjust, vjust = data$vjust,
           dx = data$dx, dy = data$dy,
           bg.r = stroke, bg.color = stroke.colour,
           gp = grid::gpar(
               col = alpha(data$colour, data$alpha),
               fontsize = data$size * .pt,
               fontfamily = data$family,
               fontface = data$fontface,
               lineheight = data$lineheight
           ),
           check.overlap = check_overlap
       )

   },

   draw_key = ggplot2::draw_key_text
)


.cont.angle <- function(x, y, gap) {
    N <- length(x)
    gap <- ceiling((gap-1)/2)
    fill <- rep(NA, gap/2)
    dx <- c(fill, diff(x, gap), fill)
    dy <- c(fill, diff(y, gap), fill)
    angle <- atan2(dy, dx)*180/pi
    angle <- ifelse(angle > 180, angle - 180, angle)
    angle <- ifelse(angle > 90, angle - 180, angle)
    angle <- ifelse(angle < -90, angle + 180, angle)
    angle
}

.label.position <- function(data, min.size, skip, rotate) {
    data <- as.data.table(data)

    breaks <- unique(data$level)
    breaks.cut <- breaks[seq(1, length(breaks), by = skip + 1)]
    data <- data[level %in% breaks.cut]
    data[, id := seq_len(.N), by = piece]
    data[, c("dx", "dy") := .(.derv(x, id), .derv(y, id)),
         by = .(piece)]

    # Safety strip around the edges (10%)
    safe <- c(0, 1) + 0.1*c(+1, -1)
    data <- data[x %between% safe &
                     y %between% safe]

    data[, N := .N, by = piece]
    data <- data[N >= min.size]
    if (nrow(data) == 0) {
        return(data)
    }
    # if (rotate == TRUE) {
        data[, angle := .cont.angle(x, y, min.size), by = piece]
    # }

    # Check if point has minimum variance
    # data[, var := minvar(x, y), by = .(piece)]
    # data[, curvature := .curvature(x, y), by = piece]
    # data <- data[data[, .I[which.min(abs(curvature))], by = piece]$V1]
    # data[is.na(var), var := FALSE]
    # data <- data[var == TRUE][, head(.SD, 1), by = piece]

    data[, min := straight(x, y, min.size), by = piece]
    # data <- data[min == TRUE]
    # data[, min := angle == min(angle, na.rm = T), by = piece]
    # data[, min := atan2(dy, dx) == min(atan2(dy, dx), na.rm = TRUE), by = piece]
    data <- data[!is.na(dx + dy)]
    if (rotate == FALSE) data[, c("dx", "dy") := .(0, 0)]

    return(data[min == TRUE, head(.SD, 1), by = piece])
}

straight <- function(x, y, gap) {
    N <- length(x)
    gap <- ceiling((gap-1)/2)
    if (N < gap) {
        gap <- N
    }
    if (is_closed(x, y)) {
        x <- c(x[(N-gap):(N-1)], x, x[2:(gap+1)])
        y <- c(y[(N-gap):(N-1)], y, y[2:(gap+1)])
        var <- vapply((gap+1):(N+gap), function(i) {
            dx <- diff(x[(i-gap):(i+gap)])
            dy <- diff(y[(i-gap):(i+gap)])
            a <- dy/dx
            var(a)
        }, FUN.VALUE = 1)
    } else {
        var <- c(rep(NA, gap), vapply((gap+1):(N-gap), function(i) {
            dx <- diff(x[(i-gap):(i+gap)])
            dy <- diff(y[(i-gap):(i+gap)])
            a <- dy/dx
            var(a)
        }, FUN.VALUE = 1), rep(NA, gap))
    }
    return(var == min(var, na.rm = TRUE))
}

# from https://stackoverflow.com/questions/21868353/drawing-labels-on-flat-section-of-contour-lines-in-ggplot2
minvar <- function (x, y){
    if (length(x) < 4) return(rep(FALSE, length(x)))
    N <- length(x)
    xdiffs <- c(NA, diff(x)) #c(NA, x[3:N] - x[1:(N-2)], NA)
    ydiffs <- c(NA, diff(y)) #c(NA, y[3:N] - y[1:(N-2)], NA)
    avgGradient <- ydiffs/xdiffs
    variance <- abs(avgGradient)
    # squareSum <- avgGradient * avgGradient
    # variance <- (squareSum - (avgGradient * avgGradient) / N) / N
    # change!! this causes problems if length(variance) < 4
    variance <- c(NA, variance[2:(N-1)], NA)
    return(variance == min(variance, na.rm = TRUE))
}
