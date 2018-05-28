#' Anomalies
#'
#' Saves keystrokes for computing anomalies.
#'
#' @param x numeric vector
#' @param ... other arguments passed to \code{\link{mean}} such as `na.rm`
#'
#' @return
#' A numeric vector of the same length as x with each value's distance to the
#' mean.
#'
#' @examples
#' # Zonal temperature anomaly
#' library(data.table)
#' temperature[, .(lon = lon, air.z = Anomaly(air)), by = .(lat, lev)]
#'
#' @family utilities
#' @export
Anomaly <- function(x, ...) {
    x - mean(x, ...)
}


#' Percentiles
#'
#' Computes percentiles.
#'
#' @param x numeric vector
#'
#' @return
#' A numeric vector of the same length as x with the percentile of each value
#' of x.
#'
#' @examples
#' x <- rnorm(100)
#' p <- Percentile(x)
#'
#' # Number of extreme temperature station-days
#' library(data.table)
#' claris <- copy(claris)
#' claris[!is.na(max), percentile := Percentile(max), by = .(id, month(date))]
#' extreme.days <- claris[, .(n = sum(percentile > 0.90, na.rm = TRUE)),
#'                        by = .(year(date))]
#' library(ggplot2)
#' ggplot(extreme.days, aes(year, n)) +
#'     geom_line()
#' @family utilities
#' @export
Percentile <- function(x) {
    ecdf(x)(x)
}




#' Magnitude of a vector
#'
#' Computes the magnitude of a vector of any dimension.
#'
#' @param ... numeric vectors of coordinates or list of coordinates
#'
#' @return
#' A numeric vector the same length as each elemenet of ...
#' that is \eqn{\sqrt(x^2 + y^2 + ...)}.
#'
#' @details
#' Helpful to save keystrokes and gain readability when computing wind
#' (or any other vector quantity) magnitude.
#'
#' @examples
#' Mag(10, 10)
#' Mag(10, 10, 10, 10)
#' Mag(list(10, 10, 10, 10))
#'
#' # There's no vector recicling!
#' \dontrun{
#' Mag(1, 1:2)
#' }
#'
#' @family utilities
#' @export
Mag <- function(...) {
    coords <- list(...)
    if (is.list(coords[[1]])) coords <- coords[[1]]
    N <- lengths(coords, use.names = FALSE)
    if (sum(N != N[1]) != 0) stop("all variables must have the same length")

    coords <- lapply(coords, `^`, 2)
    sqrt(Reduce(`+`, coords))
}



#' Extended logical operators
#'
#' Extended binary operators for easy subsetting.
#'
#' @param x,target,limits numeric vectors
#' @param tol tolerance for similarity
#' @param include logical vector of length 2 (or 1) indicating whether to include
#'  the extreme bounds
#'
#'
#' @return
#' A logical vector of the same length of x.
#'
#' @details
#' \code{\%~\%} can be thought as a "similar" operator. It's a fuzzy version of
#' \code{\link{\%in\%}} in that returns \code{TRUE} for the element of \code{x}
#' which is the (first) closest to any element of \code{target}.
#'
#' \code{Similar} is a functional version of \code{\%~\%} that also has a
#' \code{tol} parameter that indicates the maximum allowed tolerance.
#'
#' \code{\%b\%} can be thought as the "between" operator. It returns \code{TRUE}
#' for each element of \code{x} that is between the minimum and the maximum of
#' \code{limits}.
#'
#' \code{Between} is a functional version of \code{\%b\%} that also has an
#' \code{include} parameter that let's you test for \code{x > lower & x < upper}.
#' If it's a unitary vector, it will be recycled so that \code{include = TRUE} is
#' equivalent to \code{include = c(TRUE, TRUE)}.
#'
#' It's important to note that \link{data.table} already has a
#' \code{\link[data.table]{between}} function optimized with c code, so these
#' functions use that implementation if data.table is installed (except for the
#' case of \code{include[1] != include[2]}, for which data.table has no
#' implementation yet).
#'
#' @examples
#' set.seed(198)
#' x <- rnorm(100)
#' x[x %~% c(0.3, 0.5, 1)]
#'
#' # Practical use case: vertical cross-section at
#' # approximately 36W between 50S and 50N.
#' cross.lon <- -34 + 360
#' library(ggplot2)
#' ggplot(temperature[lon %~% cross.lon & lat %b% c(-50, 50)],
#'        aes(lat, lev)) +
#'     geom_contour(aes(z = air))
#'
#' @family utilities
#' @name logic
#' @export
`%~%` <- function(x, target) {
    r <- rep(FALSE, length(x))
    for (i in seq_along(target)) {
        dif <- abs(as.numeric(x - target[i]))
        x.select <- x[which.min(dif)]
        r <- r | (x == x.select)
    }
    return(r)
}

#' @rdname logic
#' @export
Similar <- function(x, target, tol = Inf) {
    r <- rep(FALSE, length(x))
    if (is.null(tol)) tol <- NA
    for (i in seq_along(target)) {
        dif <- abs(as.numeric(x - target[i]))
        x.select <- x[which.min(dif)]
        r <- r | (x == x.select & (is.na(tol) | y < abs(tol)))
    }
    return(r)
}


#' @rdname logic
#' @export
#' @import data.table
`%b%` <- function(x, limits) {
    # Operador "between"
    if (requireNamespace("data.table", quietly = TRUE)) {
        ret <- data.table::between(x, min(limits), max(limits))
    } else {
        ret <- x >= min(limits) & x <= max(limits)
    }
    return(ret)
}

#' @rdname logic
#' @export
#' @import data.table
Between <- function(x, limits, include = c(TRUE, TRUE)) {
    # Operador "between"
    if (length(include) < 2) include <- rep(include[1], 2)

    if (include[1] == include[2] & requireNamespace("data.table", quietly = TRUE)) {
        ret <- data.table::between(x, min(limits), max(limits),
                                   incbounds = include[1])
    } else {
        ret <- x > min(limits) & x < max(limits)
        if (include[1] == TRUE) {
            ret <- ret | x == min(limits)
        }
        if (include[2] == TRUE) {
            ret <- ret | x == max(limits)
        }
    }

    return(ret)
}

#' Skip observations
#'
#'
#' @param x vector
#' @param by numeric interval between elements to keep
#' @param start index to start from
#' @param fill how observations are skipped
#'
#' @return
#' A vector of the same class as x and, if \code{fill} is not \code{null},
#' the same length.
#'
#' @details
#' Mostly useful for labeling only every \code{by}th element.
#'
#' @examples
#' x <- 1:50
#' JumpBy(x, 2)   # only odd numbers
#' JumpBy(x, 2, start = 2)   # only even numbers
#' JumpBy(x, 2, fill = NA)   # even numbers replaced by NA
#' JumpBy(x, 2, fill = 6)   # even numbers replaced by 6
#'
#' @family utilities
#' @export
JumpBy <- function(x, by, start = 1, fill = NULL) {
    if (!is.null(fill)) {
        x[-seq.int(start, length(x), by = by)] <- fill
    } else {
        x <- x[seq.int(start, length(x), by = by)]
    }
    return(x)
}



is.error <- function(x) inherits(x, "try-error")

.tidy2matrix <- function(data, formula, value.var) {
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])

    g <- data.table::dcast(setDT(data), formula, value.var = value.var)

    dims <- list()
    if (length(col.vars) > 1) {
        cols <- unlist(strsplit(colnames(g), split = "_"))
    } else {
        cols <- colnames(g)
    }

    for (i in seq_along(col.vars)) {
        dims[[i]] <- JumpBy(cols, length(col.vars), start = i + length(row.vars))
        if (!.is.somedate(data[[col.vars[i]]])) {
            dims[[i]] <- as(dims[[i]], class(data[[col.vars[i]]]))
        } else {
            dims[[i]] <- as.Date(dims[[i]])
        }
    }

    # coldims <- lapply(col.vars, function(x) {
    #     u <- unique(data[[x]])
    #     u <- u[order(u)]
    # })
    # dims <- as.list(expand.grid(coldims))

    names(dims) <- col.vars

    setDF(g)
    return(list(matrix = as.matrix(g[, -seq_along(row.vars)]),
                coldims = dims,
                rowdims = as.list(g)[row.vars]))
}

# from data.table
guess <- function (x)
{
    if ("value" %chin% names(x))
        return("value")
    if ("(all)" %chin% names(x))
        return("(all)")
    var <- names(x)[ncol(x)]
    message("Using '", var, "' as value column. Use 'value.var' to override")
    return(var)
}

# from ggplot2
`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}
# from ggplot2
is.waive <- function (x) {
    inherits(x, "waiver")
}

element_render <- function(theme, element, ..., name = NULL) {

    # Get the element from the theme, calculating inheritance
    el <- ggplot2::calc_element(element, theme)
    if (is.null(el)) {
        message("Theme element ", element, " missing")
        return(ggplot2::zeroGrob())
    }

    grob <- element_grob(el, ...)
    ggname(paste(element, name, sep = "."), grob)
}

ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}


width_cm <- function(x) {
    if (is.grob(x)) {
        grid::convertWidth(grid::grobWidth(x), "cm", TRUE)
    } else if (is.unit(x)) {
        grid::convertWidth(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, width_cm, numeric(1))
    } else {
        stop("Unknown input")
    }
}
height_cm <- function(x) {
    if (is.grob(x)) {
        grid::convertHeight(grid::grobHeight(x), "cm", TRUE)
    } else if (is.unit(x)) {
        grid::convertHeight(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, height_cm, numeric(1))
    } else {
        stop("Unknown input")
    }
}

message_wrap <- function(...) {
    msg <- paste(..., collapse = "", sep = "")
    wrapped <- strwrap(msg, width = getOption("width") - 2)
    message(paste0(wrapped, collapse = "\n"))
}


#' Transform between spherical coordinates and phisical coordinates
#'
#' Transform a longitude or latitude interval into the equivalent in meters depending
#' on latitude.
#'
#' @param dx,dy interval in meters
#' @param dlon,dlat interval in degrees
#' @param lat latitude, in degrees
#' @param a radius of the Earth
#'
#' @examples
#'
#' library(data.table)
#' data(geopotential)
#' geopotential <- geopotential[date == date[1]]
#'
#' # Geostrophic wind
#' geopotential[, c("u", "v") := GeostrophicWind(gh, lon, lat)]  # in meters/second
#' geopotential[, c("dlon", "dlat") := .(dlon(u, lat), dlat(v))] # in degrees/second
#' geopotential[, c("u2", "v2") := .(dx(dlon, lat), dy(dlat))]   # again in degrees/second
#'
#' @name spherical
#' @export
dlon <- function(dx, lat, a = 6731000) {
    return(dx/(a*cos(lat*pi/180))*180/pi)
}

#' @export
#' @rdname spherical
dlat <- function(dy, a = 6731000) {
    return(dy/a*180/pi)
}

#' @export
#' @rdname spherical
dx <- function(dlon, lat, a = 6731000) {
    return(dlon*pi/180*a*cos(lat*pi/180))
}

#' @export
#' @rdname spherical
dy <- function(dlat, a = 6731000) {
    return(dlat*a*pi/180)
}

#' @importFrom stats line runif var
#' @importFrom utils head
if(getRversion() >= "2.15.1") {
    utils::globalVariables(
        c("as", "dep.names", "ecdf", "equal", "fft", "hasArg", "id",
          "ind.names", "inside", "int.level", "land", "latrad", "lon", "lonrad",
          "piece", "psi", "psi.dx", "psi.dxx", "psi.dxy", "psi.dy", "psi.dyy",
          "r2", "sd", "setTxtProgressBar", "time", "txtProgressBar",
          "u.mean", "v.mean", "write.csv", "x", "y", "z", ".", "time2",
          "group", "step", "point", "change", "end", "level", "m", "rotate",
          "x.d", "y.d", "PC", "step2", "runif", "N", "angle", "var", "head"))
}


