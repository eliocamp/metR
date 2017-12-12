#' Ideal gas law
#'
#' Calculate the density, pressure or temperature or air from the other two
#' variables.
#'
#' @param p pressure
#' @param t temperature
#' @param rho density
#' @param R gas constant for air
#'
#' @return
#' Pressure, temperature or density, according to which variable was not passed
#' as an argument.
#'
#' @details
#' Is important to take note of the units in which each variable is provided.
#' With the default value of `R`, pressure should be passed in Pascals, temperature
#' in Kelvin and density in \eqn{kg/m^3}. The result will be also in those units.
#'
#' @examples
#' IdealGas(1013*100, 20 + 273.15)
#' IdealGas(1013*100, rho = 1.15) - 273.15
#'
#' @export
#' @family meteorology functions
IdealGas <- function(p, t, rho, R = 287.058) {
    if (!hasArg(p)) {
        return(rho*R*t)
    } else if (!hasArg(rho)) {
        return(p/(R*t))
    } else if (!hasArg(t)) {
        return(p/(rho*R))
    }
}
