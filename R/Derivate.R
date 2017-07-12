#' Derivate a discrete variable using finite differences
#'
#' @param x numeric vector of discrete values
#' @param y numeric vector of locations
#' @param order order of the derivative
#' @param bc boundary conditions
#'
#' @return
#' A numeric vector of the same length as \code{x}.
#'
#' @details
#' Each element of the reuturn vector is an estimation of \eqn{dx/dy} (or
#' \eqn{d^2x/dy^2} if \code{order} = 2) by finite differences. The first and
#' last elements will be \code{NAs} unless cyclical boundary conditions are set
#' in \code{bc}.
#'
#' @examples
#' theta <- seq(0, 360, length.out = 20)*pi/180
#' x <- cos(theta)
#' dx_analytical <- -sin(theta)
#' dx_finitediff <- Derivate(x, theta)
#'
#' plot(theta, dx_analytical, type = "l")
#' points(theta, dx_finitediff, col = "red")
#' @family meteorology functions
#' @seealso \code{\link{DerivatePhysical}}
#' @export
Derivate <- function(x, y, order = c(1, 2), bc = c("cyclic", "none")) {
    N <- length(x)

    d <- y[2] - y[1]

    if (order[1] == 1) {
        dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)

    } else if (order[1] == 2) {
        dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/d^2
    }
    if (bc[1] != "cyclic") {
        dxdy[c(1, N)] <- NA
    }

    return(dxdy)
}

#' Zonal or meridional derivative
#'
#' Derivates a variable in the zonal or meridional direction taking into account
#' the size of the Earth.
#'
#' @param variable numeric vector of discrete values
#' @param lon numeric vector of longitude (in degrees)
#' @param lat numeric vector of latitude (in degrees)
#' @param order order of the derivative
#' @param bc boundary conditions
#' @param a the radius of the earth in kilometers
#'
#' @details
#' This function is usefull when using gridded data in a longitude-latitude
#' regular grid and one needs to compute zonal or meridional derivatives. If
#' \code{lon} is of length 1, it computes the meridional derivative (\eqn{d/dy})
#' at the given longitude. If \code{lat} is of length 1, then it computes
#' the zonal derivative (\eqn{d/dx}) at the given latitude.
#'
#' @return
#' A numeric vector of the same length as \code{variable}.
#'
#' @examples
#' nceptemperature[, air.dy := DerivatePhysical(air, lon = lon, lat, bc = "none"),
#'                 by = .(lev, lon)]
#'
#' ggplot(nceptemperature[lev == 500], aes(lon, lat)) +
#'     geom_tile(aes(fill = air)) +
#'     geom_contour(aes(z = air.dy, color = ..level..)) +
#'     scale_color_divergent()
#'
#' @seealso \code{\link{Derivate}}
#' @family meteorology functions
#' @export
DerivatePhysical <- function(variable, lon, lat, order = c(1, 2),
                             bc = c("cyclic", "none"), a = 6731) {
    a <- a*1000
    if (length(lon) == 1) {
        # dv/dy
        dv = Derivate(variable, lat*pi/180, order = order[1],
                      bc = bc[1])/a^order[1]
    } else {
        dv = Derivate(variable, lon*pi/180, order = order[1],
                      bc = bc[1])/(a*cos(lat*pi/180))^order[1]
    }
    return(dv)
}



