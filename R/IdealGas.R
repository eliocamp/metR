#' Thermodynamics
#'
#' Functions related to common atmospheric thermodynamic relationships.
#'
#' @param p pressure
#' @param t temperature
#' @param tv virtual temperature
#' @param td dewpoint
#' @param rho density
#' @param e vapour partial pressure
#' @param es saturation vapour partial pressure
#' @param w mixing ratio
#' @param ws saturation mixing ratio
#' @param R gas constant for air
#' @param theta potential temperature
#' @param p0 reference pressure
#' @param kappa ratio of dry air constant and specific heat capacity at constant pressure
#' @param epsilon ratio of dry air constant and vapour constant
#'
#' @return
#' Each function returns the value of the missing state variable.
#'
#' @details
#' `IdealGas` computes pressure, temperature or density of air according to the
#' ideal gas law \eqn{P=\rho R T}.
#'
#' `Adiabat` computes pressure, temperature or potential temperature according to
#' the adiabatic relationship \eqn{\theta = T (P0/P)^\kappa}.
#'
#' `VirtualTemperature` computes pressure, temperature, vapour partial pressure or
#' virtual temperature according to the virtual temperature definition
#' \eqn{T(1 - e/P(1 - \epsilon))^{-1}}.
#'
#' `MixingRatio` computes pressure, vapour partial temperature, or mixing ratio
#' according to \eqn{w = \epsilon e/(P - e)}.
#'
#' `ClausiusClapeyron` computes saturation pressure or temperature according
#' to the August-Roche-Magnus formula \eqn{es = a exp{bT/(T + c)}} with temperature
#' in Kelvin and saturation pressure in Pa.
#'
#' `DewPoint` computes pressure, saturation mixing ration or dew point
#' from the relationship \eqn{ws =  \epsilon es(Td)/(p - es(Td))}. Note that the
#' computation of dew point is approximated.
#'
#' Is important to take note of the units in which each variable is provided.
#' With the default values, pressure should be passed in Pascals, temperature and
#' potential temperature in Kelvins, and density in \eqn{kg/m^3}.
#' `ClausiusClayperon` and `DewPoint` require and return values in those units.
#'
#' The defaults value of the `R` and `kappa` parameters are correct for dry air,
#' for the case of moist air, use the virtual temperature instead of the actual
#' temperature.
#'
#' @examples
#' IdealGas(1013*100, 20 + 273.15)
#' IdealGas(1013*100, rho = 1.15) - 273.15
#'
#' (theta <- Adiabat(70000, 20 + 273.15))
#' Adiabat(70000, theta = theta) - 273.15
#'
#' # Relative humidity from T and Td
#' t <- 25 + 273.15
#' td <- 20 + 273.15
#' p <- 1000000
#' (rh <- ClausiusClapeyron(td)/ClausiusClapeyron(t))
#'
#' # Mixing ratio
#' ws <- MixingRatio(p, ClausiusClapeyron(t))
#' w <- ws*rh
#' DewPoint(p, w) - 273.15    # Recover Td
#'
#' @references
#' http://www.atmo.arizona.edu/students/courselinks/fall11/atmo551a/ATMO_451a_551a_files/WaterVapor.pdf
#'
#' @name thermodynamics
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

#' @rdname thermodynamics
#' @export
Adiabat <- function(p, t, theta, p0 = 100000, kappa = 2/7) {
    if (!hasArg(p) & hasArg(t) & hasArg(theta)) {
        return(p0*(t/theta)^(1/kappa))
    } else if (hasArg(p) & !hasArg(t) & hasArg(theta)) {
        return(theta/(p0/p)^(kappa))
    } else if (hasArg(p) & hasArg(t) & !hasArg(theta)) {
        return(t*(p0/p)^kappa)
    } else if (hasArg(p) & hasArg(t) & hasArg(theta)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}

#' @rdname thermodynamics
#' @export
VirtualTemperature <- function(p, t, e, tv, epsilon = 0.622) {
    a <- 1 - epsilon
    if (!hasArg(tv) & hasArg(p) & hasArg(t) & hasArg(e)) {
        return(t/(1 - e/p*a))
    } else if (hasArg(tv) & hasArg(p) & !hasArg(t) & hasArg(e)) {
        return(tv*(1 - e/p*a))
    } else if (hasArg(tv) & !hasArg(p) & hasArg(t) & hasArg(e)) {
        return(e*a/(1 - t/tv))
    } else if (hasArg(tv) & hasArg(p) & hasArg(t) & !hasArg(e)) {
        return(p/a*(1 - t/tv))
    } else if (hasArg(tv) & hasArg(p) & hasArg(t) & hasArg(e)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}

#' @rdname thermodynamics
#' @export
MixingRatio <- function(p, es, w, epsilon = 0.622) {
    if (hasArg(p) & !hasArg(w) & hasArg(es)) {
        return(epsilon*(es/(p - es)))
    } else if (!hasArg(p) & hasArg(w) & hasArg(es)) {
        return(es/(w*epsilon) + es)
    } else if (hasArg(p) & hasArg(w) & !hasArg(es)) {
        return(p*w*epsilon/(1 + w*epsilon))
    } else if (hasArg(p) & hasArg(w) & hasArg(es)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}

#' @rdname thermodynamics
#' @export
ClausiusClapeyron <- function(t, es) {
    a <- 6.1094*100
    b <- 17.625
    c <- 243.04

    if (hasArg(t) & !hasArg(es)) {
        t <- t - 273.15
        return(a*exp(b*t/(t + c)))
    } else if (!hasArg(t) & hasArg(es)) {
        d <- log(es/a)
        return(d*c/b/(1 - d/b) + 273.15)
    } else if (hasArg(t) & hasArg(es)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}

#' @rdname thermodynamics
#' @export
#' @importFrom stats uniroot
DewPoint <- function(p, ws, td, epsilon = 0.622) {
    if (hasArg(p) & hasArg(ws) & !hasArg(td)) {
    .dew <- function(td) {
        es <- ClausiusClapeyron(td)
        ws - epsilon*es/(p - es)
    }
    return(uniroot(.dew, c(273, 273 + 50))$root)
    } else  if (hasArg(p) & !hasArg(ws) & hasArg(td)) {
        es <- ClausiusClapeyron(td)
        return(epsilon*es/(p - es))
    } else  if (!hasArg(p) & hasArg(ws) & hasArg(td)) {
        es <- ClausiusClapeyron(td)
        return(es*(1 + epsilon/ws))
    } else if (hasArg(p) & hasArg(ws) & hasArg(td)) {
        stop("Too many state variables.")
    } else {
        stop("Too few stat variables.")
    }
}
