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
#' A numeric vector the same length as each element of ...
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
    if (any(N != N[1])) stop("all variables must have the same length")

    coords <- lapply(coords, `^`, 2)
    sqrt(Reduce(`+`, coords))
}

#' Extended logical operators
#'
#' Extended binary operators for easy subsetting.
#'
#' @param x,target numeric vectors
#' @param tol tolerance for similarity
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
#' @examples
#' set.seed(198)
#' x <- rnorm(100)
#' x[x %~% c(0.3, 0.5, 1)]
#'
#' # Practical use case: vertical cross-section at
#' # approximately 36W between 50S and 50N.
#' cross.lon <- -34 + 360
#' library(ggplot2)
#' library(data.table)
#' ggplot(temperature[lon %~% cross.lon & lat %between% c(-50, 50)],
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
        r <- r | (x == x.select & (is.na(tol) | x.select < abs(tol)))
    }
    return(r)
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




#' Transform between spherical coordinates and physical coordinates
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
