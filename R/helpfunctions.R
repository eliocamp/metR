#' Anomalies
#'
#' Saves keystrokes for computing anomalies.
#'
#' @param x numeric vector
#'
#' @return
#' A numeric vector of the same length as x with each value's distance to the
#' mena.
#'
#' @details
#' Really just a very small wraper around \code{\link{scale}} that returns a
#' numeric vector and defaults to sacale = FALSE.
#'
#' @examples
#' # Zonal temperature anomaly
#' nceptemperature[, air.z := Anomaly(air), by = .(lat, lev)]
#'
#' @family utilities
#' @export
Anomaly <- function(x) {
    as.numeric(scale(x, scale = F))
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
#' claris[!is.na(max), percentile := Percentile(max), by = .(id, month(date))]
#' extreme.days <- claris[, .(n = sum(percentile > 0.90, na.rm = TRUE)),
#'                        by = .(year(date))]
#' ggplot(extreme.days, aes(year, n)) +
#'     geom_line()
#' @family utilities
#' @export
Percentile <- function(x) {
    ecdf(x)(x)
}




#' Magnitude of a vector
#'
#' Computes the magnitude of a vector.
#'
#' @param x,y numeric vectors of vector components
#'
#' @return
#' A numeric vector the same length as x and y that is \eqn{\sqrt(x^2 + y^2)}.
#'
#' @details
#' Helpful to save keystrokes and gain readability when computing wind
#' (or any other vector quantity) magnitude.
#'
#' @family utilities
#' @export
Mag <- function(x, y) {
    sqrt(x^2 + y^2)
}

#' Extended logical operatos
#'
#' Extended binary operators for easy subsetting.
#'
#' @param x,target,limits numeric vectors
#'
#' @return
#' A logical vector of the same length of x.
#'
#' @details
#' \code{\%~\%} can be thought as a "similar" operator. It's a fuzzy version of
#' \code{\link{\%in\%}} in that returns \code{TRUE} for the element of \code{x}
#' which is the closest to any element of \code{target}. \cr
#'
#' \code{\%b\%} can be thought as the "between" operator. It returns \code{TRUE}
#' for each element of \code{x} that is between the minimum and the maximum of
#' \code{limits}.
#'
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
#' ggplot(nceptemperature[lon %~% cross.lon & lat %b% c(-50, 50)],
#'        aes(lat, lev)) +
#'     geom_contour(aes(z = air)) +
#'     scale_y_level() +
#'     scale_x_latitude()
#'
#' @family utilities
#' @name logic
#' @export
`%~%` <- function(x, target) {
    r <- rep(FALSE, length(x))
    for (i in seq_along(target)) {
        y <- abs(x - target[i])
        r <- r | (y == min(y))
    }
    return(r)
}


#' @rdname logic
#' @export
`%b%` <- function(x, limits) {
    # Operador "between"
    return(x >= min(limits) & x <= max(limits))
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
#' JumpBy(x, 2, fill = NA)   # even numbers replaced by NA
#' JumpBy(x, 2, fill = 6)   # even numbers replaced by 6
#'
#' @family utilities
#' @export
JumpBy <- function(x, by, start = 1, fill = NULL) {
    if (!is.null(fill)) {
        x[-seq(start, length(x), by = by)] <- fill
    } else {
        x <- x[seq(start, length(x), by = by)]
    }
    return(x)
}
