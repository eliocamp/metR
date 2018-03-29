#' Physical processes in the atmosphere
#'
#' Functions related to common atmospheric processes.
#'
#' @param p pressure
#' @param t temperature
#' @param rho density
#' @param R gas constant for air
#' @param tita potential temperature
#' @param p0 refference pressure
#' @param kappa ratio of dri air constant and specific heat capacity at constant pressure.
#'
#' @return
#' Each function returns the value of the missing state variable.
#'
#' @details
#' `IdealGas` computes pressure, temperature or density of air acording to the
#' ideal gas law \eqn{P=\rho R T}.
#'
#' `Adiabat` computes pressure, temperature or potential temperature acording to
#' the adiabatic relationship \eqn{\tita = T (P0/P)^\kappa}.
#'
#' Is important to take note of the units in which each variable is provided.
#' With the default values, pressure should be passed in Pascals, temperature and
#' potential temperature in Kelvins, and density in \eqn{kg/m^3}.
#' The result will be also in those units.
#'
#' The defaults value of the `R` and `kappa` parameters are correct for dry air,
#' for the case of moist air, use [GasConstant] and [Cp] or [VirtualTemperature].
#'
#' @examples
#' IdealGas(1013*100, 20 + 273.15)
#' IdealGas(1013*100, rho = 1.15) - 273.15
#'
#' (tita <- Adiabat(70000, 20 + 273.15))
#' Adiabat(70000, tita = tita) - 273.15
#'
#'
#' @name physics
#' @export
#' @family meteorology functions
IdealGas <- function(p, t, rho, R = 287.058) {
    if (!hasArg(p) & hasArg(t) & hasArg(rho)) {
        return(rho*R*t)
    } else if (hasArg(p) & hasArg(t) & !hasArg(rho)) {
        return(p/(R*t))
    } else if (hasArg(p) & !hasArg(t) & hasArg(rho)) {
        return(p/(rho*R))
    } else if (hasArg(p) & hasArg(t) & hasArg(rho)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}

#' @rdname physics
#' @export
#' @family meteorology functions
Adiabat <- function(p, t, tita, p0 = 100000, kappa = 2/7) {
    if (!hasArg(p) & hasArg(t) & hasArg(tita)) {
        return(p0*(t/tita)^(1/kappa))
    } else if (hasArg(p) & !hasArg(t) & hasArg(tita)) {
        return(tita/(p0/p)^(kappa))
    } else if (hasArg(p) & hasArg(t) & !hasArg(tita)) {
        return(t*(p0/p)^kappa)
    } else if (hasArg(p) & hasArg(t) & hasArg(tita)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}


