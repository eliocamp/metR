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
#' \dqn{d^2x/dy^2} if \code{order} = 2) by finite differences. The first and
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
#'
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
#' the size of th Earth.
#'
#' @param variable numeric vector of discrete values
#' @param lon numeric vector of longitude (in degrees)
#' @param lat numeric vector of latitude (in degrees)
#' @param order order of the derivative
#' @param bc boundary conditions
#'
#' @return
#' A numeric vector of the same length as \code{variable}.
#'
#'
#' @seealso \code{\link{Derivate}}
#' @export
DerivatePhysical <- function(variable, lon, lat, order = c(1, 2),
                             bc = c("cyclic", "none"), a = 6731) {
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
