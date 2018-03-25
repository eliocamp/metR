#' @rdname geom_contour_fill
#' @family ggplot2 helpers
#' @export
#' @import sp
#' @import ggplot2
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "ContourFill", position = "identity",
                              ...,
                              breaks = NULL,
                              bins = NULL,
                              binwidth = NULL,
                              na.rm = FALSE,
                              exclude = NULL,
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
            exclude = exclude,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            ...
        )
    )
}

#' @import ggplot2
#' @import scales
StatContourFill <- ggplot2::ggproto("StatContourFill", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    default_aes = ggplot2::aes(fill = ..int.level..),

    compute_layer = function(self, data, params, layout) {
        ggplot2:::check_required_aesthetics(
            self$required_aes,
            c(names(data), names(params)),
            ggplot2:::snake_class(self)
            )

        # Trim off extra parameters
        params <- params[intersect(names(params), self$parameters())]

        args <- c(list(data = quote(data), scales = quote(scales)), params)
        plyr::ddply(data, "PANEL", function(data) {
            scales <- layout$get_scales(data$PANEL[1])
            tryCatch(do.call(self$compute_panel, args), error = function(e) {
                warning("Computation failed in `", ggplot2:::snake_class(self), "()`:\n",
                        e$message, call. = FALSE)
                data.frame()
            })
        })
    },
    compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                             breaks = scales::fullseq, complete = TRUE, na.rm = FALSE,
                             exclude = NULL) {
        data <- data[!(is.na(data$x) | is.na(data$y)), ]
# dd <<- data
        if (na.rm) {
            data <- data[!is.na(data$z), ]
        } else {
            data$z[is.na(data$z)] <- mean(data$z, na.rm = T)
        }

        # Check is.null(breaks) for backwards compatibility
        if (is.null(breaks)) {
            breaks <- scales::fullseq
        }

        if (is.function(breaks)) {
            # If no parameters set, use pretty bins to calculate binwidth
            if (is.null(bins) && is.null(binwidth)) {
                binwidth <- diff(pretty(range(data$z), 10))[1]
            }
            # If provided, use bins to calculate binwidth
            if (!is.null(bins)) {
                binwidth <- diff(range(data$z)) / bins
            }

            breaks <- breaks(range(data$z), binwidth)
        }

        if (!is.null(exclude)) {
            warning("argumnet exclude is deprecated; use a function in breaks instead.",
                    call. = FALSE)
            breaks <- breaks[!(breaks %in% exclude)]
        }
        # breaks.inner <- .inside(breaks)

        mean.z <- mean(data$z)
        mean.level <- breaks[breaks %~% mean.z]
        range.data <- as.data.frame(sapply(data[c("x", "y", "z")], range))

        # Expand data by 1 unit in all directions.
        data <- .expand_data(data)

        # Make contours
        cont <- data.table::setDT(ggplot2:::contour_lines(data, breaks, complete = complete))
        if (length(cont) == 0) return(cont)

        # Ugly hack for joining disjointed contours
        cont <- .join_contours(cont)

        # Calculate inner fill
        cont <- .inner_fill(cont, data, breaks)

        i <-  which(breaks == mean.level)
        correction <- sign(mean.z - mean.level)
        if (correction == 0) correction <- 1
        correction <- (breaks[i + correction] - mean.level)/2

        if (mean.level %in% breaks & complete == TRUE) {
            mean.cont  <- data.frame(
                level = mean.level,
                x = c(rep(range.data$x[1], 2), rep(range.data$x[2], 2)),
                y = c(range.data$y[1], rep(range.data$y[2], 2), range.data$y[1]),
                piece = max(cont$piece) + 1,
                int.level = mean.level + correction)
            mean.cont$group <- factor(paste("", sprintf("%03d", mean.cont$piece), sep = "-"))
            cont <- rbind(cont, mean.cont)
        }

        # Move contours to range of original data
        cont$x[cont$x > range.data$x[2]] <- range.data$x[2]
        cont$x[cont$x < range.data$x[1]] <- range.data$x[1]
        cont$y[cont$y < range.data$y[1]] <- range.data$y[1]
        cont$y[cont$y > range.data$y[2]] <- range.data$y[2]

        # Order pieces according to area.
        cont <- .order_fill(cont)

        # Sometimes there are extra contours.
        cont <- cont[int.level %between% range(breaks)]

        cont
        }
)

.order_fill <- function(cont) {
    areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
        , rank := frank(-area, ties.method = "first")]

    cont <- cont[areas, on = "piece"]
    area.back <- areas[piece == max(cont$piece), area]
    if (any(areas[piece != max(cont$piece), area] == area.back)) {
        cont <- cont[piece != max(cont$piece)]
    }
    cont[, piece := rank]
    cont[, group := factor(paste("", sprintf("%03d", piece), sep = "-"))]
    cont
}

.expand_data <- function(data) {
    dx <- ggplot2::resolution(subset(data, y == data$y[1])$x)
    # dx <- abs(diff(subset(data, y == data$y[1])$x)[1])
    dy <- ggplot2::resolution(subset(data, x == data$x[1])$y)
    # dy <- abs(diff(subset(data, x == data$x[1])$y)[1])

    #Extender para grilla rectangular.
    range.data <- as.data.frame(sapply(data[c("x", "y", "z")], range))
    # r2 <<- range.data
    extra <- rbind(expand.grid(y = c(range.data$y[2] + dy,
                                     range.data$y[1] - dy),
                               x = unique(data$x)),
                   expand.grid(y = c(unique(data$y), range.data$y[2] + dy,
                                     range.data$y[1] - dy),
                               x = c(range.data$x[1] - dx, range.data$x[2] + dx)))

    extra$z <- mean(data$z)

    rbind(data[c("x", "y", "z")], extra)
}

# area
# clockwise > 0, counterclockwise < 0
# From http://stackoverflow.com/questions/1165647
area <- function(x, y) {
    xdiff <- c(x[-1], x[1]) - x
    ysum <- c(y[-1], y[1]) + y
    sum(xdiff * ysum)/2
}


#' @import data.table
.inner_fill <- function(cont, data, breaks) {
    # Add one more to breaks extremes
    m.level <- -breaks[2] + 2*breaks[1]
    M.level <- 2*breaks[length(breaks)] - breaks[length(breaks) - 1]
    breaks <- c(m.level, breaks, M.level)

    cont[, int.level := 0]
    pieces <- unique(cont$piece)

    # Data properties
    data <- data.table::as.data.table(data)
    x.data <- unique(data$x)
    x.data <- x.data[order(x.data)]
    x.N <- length(x.data)
    y.data <- unique(data$y)
    y.data <- y.data[order(y.data)]
    y.N <- length(y.data)

    # Make correction for each piece
    for (p in pieces) {
        level <- cont[piece == p, level[1]]

        i <- which(breaks == level)
        cur.piece <- cont[piece == p]

        # Get one point in piece
        p0 <- cur.piece[x >= x.data[2] & x <= x.data[x.N-1]
                        & y >= y.data[2] & y <= y.data[y.N-1]][1]

        if (nrow(p0[!is.na(x)]) == 0) {
            inside.z <- level
        } else {
            # If p0 is in the x grid of the data
            if (p0$x %in% x.data) {
                # Search for the closest data points up and down
                y1 <- Closest(y.data, p0$y, sign = 1)
                y2 <- Closest(y.data, p0$y, sign = -1)
                p1 <- data[x == p0$x & y == y1]
                p2 <- data[x == p0$x & y == y2]
            # If p0 is in the y grid of the data
            } else {
                # Search for the closest data points left and right
                x1 <- Closest(x.data, p0$x, sign = 1)
                x2 <- Closest(x.data, p0$x, sign = -1)
                p1 <- data[x == x1 & y == p0$y]
                p2 <- data[x == x2 & y == p0$y]
            }

            points <- rbind(p1, p2)
            # Get one point whose value is NOT equal to the contour level.
            points[, equal := z == level]
            points <- points[equal == FALSE][1]
            points[, inside := IsInside(x, y, cur.piece$x, cur.piece$y)]
        }

        correction <- (breaks[i + sign(points$z - level)] - level)/2

        # Change sign of correction if point is outside.
        corr.sign <- as.numeric(points$inside)*2 - 1
        correction <- correction*corr.sign

        cont[piece == p, int.level := level + correction]
    }
    return(cont)
}


Closest <- function(x, target, sign = c(1, -1)) {
    tmp <- (x - target)*sign[1]
    tmp[tmp < 0] <- NA
    x[which.min(abs(tmp))]
}


IsInside <- function(xp, yp, x, y) {
    !(sp::point.in.polygon(xp, yp, x, y) == 0)
}

is_closed <- function(x, y) {
    (x[length(x)] == x[1]) & (y[length(x)] == y[1])
}


close_path <- function(x, y, range_x, range_y) {
    L <- length(x)
    if ((x[1] == x[L] & x[1] %in% range_x) |
        (y[1] == y[L] & y[1] %in% range_y)) {

    } else if (sum(x %in% range_x)*sum(y %in% range_y) != 0) {
        x[L + 1] <- x[x %in% range_x]
        y[L + 1] <- y[y %in% range_y]
    } else if (sum(y %in% range_y) == 0) {
        x[L + 1] <- x[L]
        y[L + 1] <- range_y[1]    # arbitrary

        y[L + 2] <- y[L + 1]
        x[L + 2] <- x[1]
    } else if (sum(x %in% range_x) == 0) {
        y[L + 1] <- y[L]
        x[L + 1] <- range_x[1]    # arbitrary

        y[L + 2] <- y[1]
        x[L + 2] <- x[L + 1]
    }
    L <- length(x)
    x[L + 1] <- x[1]
    y[L + 1] <- y[1]
    return(list(x = x, y = y))
}


.join_contours <- function(contours) {
    #join small segments
    # For now, lets asume that the problem occurs with only 1 contour.
    # If the case arises that more than one contour is separated into pieces,
    # it will be a problem for my future self.
    # (Oh, hi, future self!)

    contours[, close := is_closed(x, y), by = piece]
    contours2 <- contours[close == FALSE][, close := NULL]
    if (length(contours2) == 0) return(contours)
    contours2[, first := (1 == 1:.N), by = piece]
    pieces <- unique(contours2[, piece])

    cont <- contours2[piece == pieces[1]]
    p <- pieces[1]
    for(i in seq_along(pieces)) {
        last <- cont[, .(x = x[.N], y = y[.N])]
        # search for next piece
        next.point <- contours2[x == last$x & y == last$y & piece != p]
        next.piece <- contours2[piece == next.point$piece]

        if (next.piece$piece[1] == pieces[1]) break

        cont <- rbindlist(list(cont, .reverse(next.piece, next.point)))
        p <- next.piece[1, piece]
    }
    cont[, piece := max(contours$piece) + 1]
    cont <- rbindlist(list(contours[close == TRUE], cont))
    cont[, close := NULL]
    cont
}


.reverse <- function(cont, point) {
    if (identical(cont[1], point)) {
        return(cont)
    } else {
        return(cont[.N:1])
    }
}




