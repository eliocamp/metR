#' @rdname geom_contour_fill
#' @export
stat_contour_fill <- function(mapping = NULL, data = NULL,
                              geom = "polygon", position = "identity",
                              ...,
                              breaks = MakeBreaks(),
                              bins = NULL,
                              binwidth = NULL,
                              global.breaks = TRUE,
                              kriging = FALSE,
                              na.fill = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
    .check_wrap_param(list(...))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatContourFill,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = FALSE,
            na.fill = na.fill,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            global.breaks = global.breaks,
            kriging = kriging,
            ...
        )
    )
}

#' @rdname geom_contour_fill
#' @usage NULL
#' @format NULL
#' @export
StatContourFill <- ggplot2::ggproto("StatContourFill", ggplot2::Stat,
    required_aes = c("x", "y", "z"),
    default_aes = ggplot2::aes(fill = ..level_mid.., order = ..level..),
    setup_params = function(data, params) {
        if (is.null(params$global) || isTRUE(params$global.breaks)) {
            params$breaks <- setup_breaks(data,
                                          breaks = params$breaks,
                                          bins = params$bins,
                                          binwidth = params$binwidth)
        }
        return(params)
    },
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
                warning("Computation failed in `", ggplot2:::snake_class(self),
                        "()`:\n",
                        e$message, call. = FALSE)
                data.frame()
            })
        })
    },
    compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                             breaks = scales::fullseq, complete = TRUE,
                             na.rm = FALSE, xwrap = NULL,
                             ywrap = NULL, na.fill = FALSE, global.breaks = TRUE,
                             proj = NULL, kriging = FALSE) {
        data.table::setDT(data)

        if (isFALSE(global.breaks)) {
            breaks <- setup_breaks(data,
                                   breaks = breaks,
                                   bins = bins,
                                   binwidth = binwidth)
        }

        data <- data[!(is.na(y) | is.na(x)), ]

        if (!isFALSE(na.fill)) {
            # Check if is a complete grid
            complete.grid <- with(data, .is.regular_grid(x, y))
            if (complete.grid == FALSE) {
                if (complete == FALSE) {
                    warning("data must be a complete regular grid", call. = FALSE)
                    return(data.frame())
                } else {
                    # data <- data.table::setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
                    data <- .complete(data, x, y)
                }
            }

            data <- .impute_data(data, na.fill)
        } else {
            data <- data[!is.na(z), ]
        }

        if (kriging) {
            check_packages("kriging", "kriging")

            pixels <- 40
            data <- try(with(data, setNames(kriging::kriging(x, y, z, pixels = pixels)$map,
                                            c("x", "y", "z"))), silent = TRUE)
            if (inherits(data, "try-error")) {
                warning("kriging failed. Perhaps the number of points is too small.")
                return(data.frame())
            }

            data.table::setDT(data)
        }

        if (!is.null(xwrap)) {
            data <- suppressWarnings(WrapCircular(data, "x", xwrap))
        }
        if (!is.null(ywrap)) {
            data <- suppressWarnings(WrapCircular(data, "y", ywrap))
        }


        # Make contours
        cont <- data.table::setDT(.contour_bands(data, breaks, complete = complete))

        cont[, int.level := (level_high + level_low)/2]
        cont[, level_mid := int.level]
        cont[, nlevel := level_high/max(level_high)]

        if (!is.null(proj)) {
            if (is.function(proj)) {
                cont <- proj(cont)
            } else {
                if (is.character(proj)) {
                    if (!requireNamespace("proj4", quietly = TRUE)) {
                        stop("Projection requires the proj4 package. Install it with `install.packages(\"proj4\")`")
                    }
                    cont <- data.table::copy(cont)[, c("x", "y") := proj4::project(list(x, y), proj,
                                                                                           inverse = TRUE)][]

                }
            }
        }

        cont

        }
)



.contour_bands <- function(data, breaks, complete = FALSE) {
    band <- level_high <- level_low <- NULL

    # From ggplot2
    x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
    y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

    nrow <- max(y_pos)
    ncol <- max(x_pos)

    z <- matrix(NA_real_, nrow = nrow, ncol = ncol)
    z[cbind(y_pos, x_pos)] <- data$z



    cl <- isoband::isobands(x = sort(unique(data$x)),
                            y = sort(unique(data$y)),
                            z = z,
                            levels_low = breaks[-length(breaks)],
                            levels_high = breaks[-1])


    if (length(cl) == 0) {
        warning("Not possible to generate contour data", call. = FALSE)
        return(data.frame())
    }

    # Convert list of lists into single data frame

    cont <- data.table::rbindlist(lapply(cl, data.table::as.data.table), idcol = "band")

    cont[, band := ordered(band, names(cl))]
    cont[, c("level_low", "level_high") := data.table::tstrsplit(band, ":")]
    cont[, `:=`(level_low = as.numeric(level_low), level_high = as.numeric(level_high))]
    # cont[, level := paste0("(", format(as.numeric(level_low), digits = 3, trim = TRUE),
    #                        ", ", format(as.numeric(level_high), digits = 3, trim = TRUE))]
    # cont[, level := factor(level, ordered = TRUE)]

    cont[, piece := as.numeric(interaction(band))]
    cont[, group := factor(paste(data$group[1], sprintf("%03d", piece), sep = "-"))]


    cont[, .(level = band, level_low, level_high, x, y, piece, group, subgroup = id)]

}

.order_fill <- function(cont) {
    areas <- cont[, .(area = abs(area(x, y))), by = .(piece)][
        , rank := data.table::frank(-area, ties.method = "first")]

    cont <- cont[areas, on = "piece"]
    area.back <- areas[piece == max(cont$piece), area]
    if (any(areas[piece != max(cont$piece), area] == area.back)) {
        cont <- cont[piece != max(cont$piece)]
    }
    cont[, piece := rank]
    cont[, group := factor(paste("", sprintf("%03d", piece), sep = "-"))]
    cont
}

.expand_data <- function(data, fill) {
    dx <- ggplot2::resolution(subset(data, y == data$y[1])$x)
    dy <- ggplot2::resolution(subset(data, x == data$x[1])$y)

    #Extender para grilla rectangular.
    range.data <- as.data.frame(vapply(data[c("x", "y", "z")], range, c(1,2)))
    extra <- rbind(expand.grid(y = c(range.data$y[2] + dy,
                                     range.data$y[1] - dy),
                               x = unique(data$x)),
                   expand.grid(y = c(unique(data$y), range.data$y[2] + dy,
                                     range.data$y[1] - dy),
                               x = c(range.data$x[1] - dx, range.data$x[2] + dx)))

    extra$z <- fill

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

        # Change sign of correction if point is outside.
        corr.sign <- as.numeric(points$inside)*2 - 1

        correction <- (breaks[i + sign(points$z - level)*corr.sign] - level)/2

        cont[piece == p, int.level := level + correction]
    }
    return(cont)
}

.inner_fill.m <- memoise::memoise( .inner_fill)

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

# nocov start
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
# nocov end


.join_contours <- function(contours) {
    #join small segments
    # For now, lets asume that the problem occurs with only 1 contour.
    # If the case arises that more than one contour is separated into pieces,
    # it will be a problem for my future self.
    # (Oh, hi, future self!)

    contours[, close := is_closed(x, y), by = piece]
    contours2 <- contours[close == FALSE]
    contours[, close := NULL]
    if (nrow(contours2) == 0) return(contours)
    contours2[, close := NULL]
    first <- NULL
    contours2[, first := (1 == 1:.N), by = piece]
    pieces <- unique(contours2[, piece])

    cont <- contours2[piece == pieces[1]]
    p <- pieces[1]
    for(i in seq_along(pieces)) {
        last <- cont[, .(x = x[.N], y = y[.N])]
        # search for next piece
        next.point <- contours2[x == last$x & y == last$y & piece != p]

        if (nrow(next.point) == 0) {
            warning("some contours may not have closed correctly", call. = FALSE)
            return(contours)
        }

        # if (nrow(next.point) == 0) break
        next.piece <- contours2[piece == next.point$piece]

        if (next.piece$piece[1] == pieces[1]) break

        cont <- data.table::rbindlist(list(cont, .reverse(next.piece, next.point)))
        p <- next.piece[1, piece]
    }
    cont[, piece := max(contours$piece) + 1]
    cont <- data.table::rbindlist(list(contours[close == TRUE], cont))
    cont[, close := NULL]
    cont
}

.join_contours.m <- memoise::memoise(.join_contours)


.reverse <- function(cont, point) {
    if (identical(cont[1], point)) {
        return(cont)
    } else {
        return(cont[.N:1])
    }
}




