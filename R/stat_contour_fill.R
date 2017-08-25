#' Filled 2d contours of a 3d surface
#'
#' While ggplot2's \code{\link[ggplot2]{stat_contour}} can plot nice contours, it
#' doesn't work with the polygon geom. This stat makes some small manipulation
#' of the data to ensure that all contours are closed and also computes a new
#' aesthetic \code{int.level}, which differs from \code{level} (computed by
#' [ggplot2::stat_contour]) in that represents
#' the value of the \code{z} aesthetic *inside* the contour instead of at the edge.
#'
#'
#' @section Computed variables:
#' \describe{
#'  \item{int.level}{value of the interior contour}
#'  }
#' @examples
#' library(ggplot2)
#' surface <- reshape2::melt(volcano)
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   stat_contour_fill() +
#'   geom_contour(color = "black", size = 0.1)
#'
#' # If one uses level instead of int.level, one of the small
#' # contours near the crater disapears
#' ggplot(surface, aes(Var1, Var2, z = value)) +
#'   stat_contour_fill(aes(fill = ..level..))
#'
#'
#'
#' @family ggplot2 helpers
#' @export
#' @import sp
#' @import ggplot2
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatContourFill,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}

#' @import ggplot2
StatContourFill <- ggplot2::ggproto("StatContourFill", ggplot2::Stat,
                           required_aes = c("x", "y", "z"),
                           default_aes = ggplot2::aes(fill = ..int.level..),

                           compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                                                    breaks = NULL, complete = FALSE, na.rm = FALSE,
                                                    exclude = NA) {
                               # If no parameters set, use pretty bins
                               if (is.null(bins) && is.null(binwidth) && is.null(breaks)) {
                                   breaks <- pretty(range(data$z), 10)
                               }
                               # If provided, use bins to calculate binwidth
                               if (!is.null(bins)) {
                                   binwidth <- diff(range(data$z)) / bins
                               }
                               # If necessary, compute breaks from binwidth
                               if (is.null(breaks)) {
                                   breaks <- fullseq(range(data$z), binwidth)
                               }

                               # if (is.null(binwidth)) {
                               #     binwidth <- diff(range(data$z)) / length(breaks)
                               # }
                               # library(data.table)

                               breaks.keep <- breaks[!(breaks %in% exclude)]
                               #f <<- data    # debug
                               dx <- abs(diff(subset(data, y == data$y[1])$x)[1])
                               dy <- abs(diff(subset(data, x == data$x[1])$y)[1])

                               #Extender para grilla rectangular.
                               range.data <- as.data.frame(sapply(data[c("x", "y", "z")], range))

                               extra <- rbind(
                                   expand.grid(y = c(range.data$y[2] + dy, range.data$y[1] - dy),
                                               x = unique(data$x)),
                                   expand.grid(y = c(unique(data$y), range.data$y[2] + dy, range.data$y[1] - dy),
                                               x = c(range.data$x[1] - dx, range.data$x[2] + dx))
                               )

                               ## Extender cualqueir grilla
                               # setDT(data)
                               # extra.x <- data[ ,  .(x = c(max(x) + dx, min(x) - dx)), by = y]
                               # extra.y <- data[ ,  .(y = c(max(y) + dy, min(y) - dy)), by = x]
                               # extra   <- rbind(extra.y, extra.x)

                               mean.z <- mean(data$z)
                               mean.level <- breaks[breaks %~% mean.z]
                               extra$z <- mean.z
                               # extra$PANEL <- data$PANEL[1]
                               cur.group <- data$group[1]
                               # extra$group <- data$group[1]

                               # dbug.data <<- copy(data)
                               # dbug.extra <<- copy(extra)
                               data2 <- rbind(data[c("x", "y", "z")], extra)

                               cont <- ggplot2:::contour_lines(data2, breaks.keep, complete = complete)

                               data.table::setDT(cont)

                               # co <<- copy(cont)    # debug
                               #data3 <<- data2    # debug
                               cont <- CorrectFill(cont, data2, breaks)


                               i <-  which(breaks.keep == mean.level)
                               correction <- (breaks.keep[i + sign(mean.z - mean.level)] - mean.level)/2
                               # correction <- 0

                               if (mean.level %in% breaks.keep) {
                                   mean.cont  <- data.frame(
                                       level = mean.level,
                                       x = c(rep(range.data$x[1], 2), rep(range.data$x[2], 2)),
                                       y = c(range.data$y[1], rep(range.data$y[2], 2), range.data$y[1]),
                                       piece = max(cont$piece) + 1,
                                       int.level = mean.level + correction)

                                   mean.cont$group <- factor(paste(cur.group, sprintf("%03d", mean.cont$piece), sep = "-"))
                                   cont <- rbind(cont, mean.cont)
                               }
                               #co.2 <<- copy(cont)    # debug

                               areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
                                   , rank := frank(-area, ties.method = "dense")]
                               areas <- areas[, head(.SD, 1), by = piece]
                               cont <- cont[areas, on = "piece"]
                               cont[, piece := rank]
                               cont[, group := factor(paste(cur.group,
                                                            sprintf("%03d", piece), sep = "-"))]

                               cont$x[cont$x > range.data$x[2]] <- range.data$x[2]
                               cont$x[cont$x < range.data$x[1]] <- range.data$x[1]
                               cont$y[cont$y < range.data$y[1]] <- range.data$y[1]
                               cont$y[cont$y > range.data$y[2]] <- range.data$y[2]

                               cont
                           }

)


# From https://stat.ethz.ch/pipermail/r-help/2004-December/063046.html
area <- function(x, y){
    X <- matrix(c(x, y), ncol = 2)
    X <- rbind(X,X[1,])
    x <- X[,1]
    y <- X[,2]
    lx <- length(x)
    -sum((x[2:lx] - x[1:lx-1])*(y[2:lx] + y[1:lx-1]))/2
}

#' @import data.table
CorrectFill <- function(cont, data, breaks) {
    levels <- breaks
    m.level <- -levels[2] + 2*levels[1]
    M.level <- 2*levels[length(levels)] - levels[length(levels) - 1]
    levels <- c(m.level, levels, M.level)
    cont[, int.level := 0]
    pieces <- unique(cont$piece)

    data <- data.table::as.data.table(data)
    x.data <- unique(data$x)
    x.data <- x.data[order(x.data)]
    x.N <- length(x.data)
    # range.x <- range(data$x)
    y.data <- unique(data$y)
    y.data <- y.data[order(y.data)]
    y.N <- length(y.data)
    # range.y <- range(data$y)

    for (p in pieces) {
        level <- cont[piece == p, level[1]]

        i <- which(levels == level)
        cur.piece <- cont[piece == p]

        p0 <- cur.piece[x >= x.data[2] & x <= x.data[x.N-1]
                        & y >= y.data[2] & y <= y.data[y.N-1]][1]
        if (nrow(p0[!is.na(x)]) == 0) {
            inside.z <- level
        } else {
            if (p0$x %in% x.data) {
                y1 <- Closest(y.data, p0$y, sign = 1)
                y2 <- Closest(y.data, p0$y, sign = -1)
                p1 <- data[x == p0$x & y == y1]
                p2 <- data[x == p0$x & y == y2]
            } else {
                x1 <- Closest(x.data, p0$x, sign = 1)
                x2 <- Closest(x.data, p0$x, sign = -1)

                p1 <- data[x == x1 & y == p0$y]
                p2 <- data[x == x2 & y == p0$y]
            }

            points <- rbind(p1, p2)
            # Get one point whose value is NOT equal to the contour level.
            points[, equal := (z == cur.piece$level[1])]
            points <- points[equal == FALSE][1]

            points[, inside := IsInside(x, y, cur.piece$x, cur.piece$y)]

            # if (IsInside(p1$x, p1$y, cur.piece$x, cur.piece$y)) {
            #     inside.z <- p1$z
            # } else {
            #     inside.z <- p2$z
            # }
        }

        correction <- (levels[i + sign(points$z - level)] - level)/2

        # Change sign of correction if point is outside.
        corr.sign <- as.numeric(points$inside)*2 - 1
        correction <- correction*corr.sign

        cont[piece == p, int.level := level + correction]
    }
    return(cont)
}


Closest <- function(x, target, sign = c(1, -1)) {
    tmp <- (x - target)*sign[1]
    tmp[tmp<0] <- NA
    x[which.min(abs(tmp))]
}


IsInside <- function(xp, yp, x, y) {
    !(sp::point.in.polygon(xp, yp, x, y) == 0)
}

