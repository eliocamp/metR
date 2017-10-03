#' Calculate wave-activity flux
#'
#' @param gh geopotential height
#' @param u mean zonal velocity
#' @param v mean meridional velocity
#' @param lon longitude (in degrees)
#' @param lat latitude (in degrees)
#' @param lev pressure level (in hPa)
#'
#' @details
#' Calculates Plum-like wave activity fluxes
#'
#' @return
#' A list with elements: longitude, latitude, and the two horizontal components
#' of the wave activity flux.
#'
#' @references
#' Takaya, K. and H. Nakamura, 2001: A Formulation of a Phase-Independent Wave-Activity Flux for Stationary and Migratory Quasigeostrophic Eddies on a Zonally Varying Basic Flow. J. Atmos. Sci., 58, 608–627, \url{https://doi.org/10.1175/1520-0469(2001)058<0608:AFOAPI>2.0.CO;2} \cr
#' Adapted from \url{https://github.com/marisolosman/Reunion_Clima/blob/master/WAF/Calculo_WAF.ipynb}
#' @family meteorology functions
#' @export
#' @import data.table
WaveFlux <- function(gh, u, v, lon, lat, lev) {
    g  <- 9.81
    a <- 6371000
    p0 <- 100000    # normalizo a 100hPa

    # Todo en una data.table para que sea más cómodo.
    dt <- data.table(lon = lon, lat = lat,
                     lonrad = lon*pi/180, latrad = lat*pi/180,
                     gh = gh, u.mean = u, v.mean = v)
    setkey(dt, lat, lon)
    dt[, f := 2*pi/(3600*24)*sin(latrad)]
    dt[, psi := g/f*gh]

    # Derivadas
    dt[, `:=`(psi.dx  = Derivate(psi, lonrad),
              psi.dxx = Derivate(psi, lonrad, 2)), by = lat]
    dt[, `:=`(psi.dy  = Derivate(psi, latrad, bc = "none"),
              psi.dyy = Derivate(psi, latrad, 2, bc = "none"),
              psi.dxy = Derivate(psi.dx, latrad, bc = "none")), by = lon]

    # Cálculo del flujo (al fin!)
    flux <- dt[, {
        wind <- sqrt(u.mean^2 + v.mean^2)

        xu <- psi.dx^2      - psi*psi.dxx
        xv <- psi.dx*psi.dy - psi*psi.dxy
        yv <- psi.dy^2      - psi*psi.dyy

        coslat <- cos(latrad)
        coeff <- lev*100/p0/(2*wind*a^2)

        w.x <- coeff*(u.mean/coslat*xu + v.mean*xv)
        w.y <- coeff*(u.mean*xv + v.mean*coslat*yv)

        list(lon = lon, lat = lat,
             w.x = w.x, w.y = w.y)
        }]
    return(flux)
}
