#' Calculate wave-activity flux
#'
#' @param gh geopotential height
#' @param u mean zonal velocity
#' @param v mean meridional velocity
#' @param lon longitude (in degrees)
#' @param lat latitude (in degrees)
#' @param lev pressure level (in hPa)
#' @param g acceleration of gravity
#' @param a Earth's radius
#'
#' @details
#' Calculates Plum-like wave activity fluxes
#'
#' @return
#' A list with elements: longitude, latitude, and the two horizontal components
#' of the wave activity flux.
#'
#' @references
#' Takaya, K. and H. Nakamura, 2001: A Formulation of a Phase-Independent Wave-Activity Flux for Stationary and Migratory Quasigeostrophic Eddies on a Zonally Varying Basic Flow. J. Atmos. Sci., 58, 608–627, \doi{10.1175/1520-0469(2001)058<0608:AFOAPI>2.0.CO;2} \cr
#' Adapted from \url{https://github.com/marisolosman/Reunion_Clima/blob/master/WAF/Calculo_WAF.ipynb}
#' @family meteorology functions
#' @export
WaveFlux <- function(gh, u, v, lon, lat, lev, g = 9.81, a = 6371000) {
    checks <- makeAssertCollection()
    assertNumeric(gh, add = checks)
    assertNumeric(u, add = checks)
    assertNumeric(v, add = checks)
    assertNumeric(lon, add = checks)
    assertNumeric(lat, add = checks)
    assertNumber(lev, add = checks)
    lengths <- c(gh = length(gh), lon = length(lon), lat = length(lat),
                 y = length(u), v = length(v))
    assertSameLength(lengths, add = checks)
    assertNumber(g, finite = TRUE, add = checks)
    assertNumber(a, finite = TRUE, add = checks)
    reportAssertions(checks)

    p0 <- 100000    # normalizo a 100hPa

    # Todo en una data.table para que sea más cómodo.
    dt <- data.table::data.table(lon = lon, lat = lat,
                     lonrad = lon*pi/180, latrad = lat*pi/180,
                     gh = gh, u.mean = u, v.mean = v)
    data.table::setkey(dt, lat, lon)
    dt[, f := 2*pi/(3600*24)*sin(latrad)]
    dt[, psi := g/f*gh]

    # Derivadas
    dt[, `:=`(psi.dx  = Derivate(psi ~ lonrad, cyclical = TRUE)[[1]],
              psi.dxx = Derivate(psi ~ lonrad, 2, cyclical = TRUE)[[1]]), by = lat]
    dt[, `:=`(psi.dy  = Derivate(psi ~ latrad, cyclical = FALSE)[[1]],
              psi.dyy = Derivate(psi ~ latrad, 2, cyclical = FALSE)[[1]],
              psi.dxy = Derivate(psi.dx ~ latrad, cyclical = FALSE)[[1]]), by = lon]

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

        list(
             w.x = w.x, w.y = w.y)
        }]
    return(flux)
}



#' Computes Eliassen-Palm fluxes.
#'
#' @param lon longitudes in degrees.
#' @param lat latitudes in degrees.
#' @param lev pressure levels.
#' @param t temperature in Kelvin.
#' @param u zonal wind in m/s.
#' @param v meridional wind in m/s.
#'
#' @return
#' A data.table with columns `Flon`, `Flat` and `Flev` giving the zonal, meridional
#' and vertical components of the EP Fluxes at each longitude, latitude and level.
#'
#' @references
#' Plumb, R. A. (1985). On the Three-Dimensional Propagation of Stationary Waves. Journal of the Atmospheric Sciences, 42(3), 217–229. \doi{10.1175/1520-0469(1985)042<0217:OTTDPO>2.0.CO;2}
#' Cohen, J., Barlow, M., Kushner, P. J., & Saito, K. (2007). Stratosphere–Troposphere Coupling and Links with Eurasian Land Surface Variability. Journal of Climate, 20(21), 5335–5343. \doi{10.1175/2007JCLI1725.1}
#' @export
EPflux <- function(lon, lat, lev, t, u, v) {
    # Ecuación 7.1 plumb 1984
    # https://journals.ametsoc.org/doi/pdf/10.1175/1520-0469%281985%29042%3C0217%3AOTTDPO%3E2.0.CO%3B2
    a <-  6371000
    H <- 8000

    data <- data.table::data.table(
        lon = lon,
        lat = lat,
        lev = lev,
        t = t,
        u = u,
        v = v)

    # Cáclulo de S: Cohen et.al.
    # https://journals.ametsoc.org/doi/pdf/10.1175/2007JCLI1725.1

    data[, tita := Adiabat(lev, t) ][
         , dtp := .derv(t, lev), by = .(lon, lat) ][
         , `:=`(S = mean(-lev*H*dtp + 2/7*t/H, na.rm = TRUE)),
            by = .(lev, sign(lat)) ][
         , `:=`(tita_z = Anomaly(tita),
                 t_z = Anomaly(t),
                 u_z = Anomaly(u),
                 v_z = Anomaly(v)),
            by = .(lev, lat)][
         , `:=`(vtita = v_z*tita_z,
                 utita = u_z*tita_z,
                 vt = v_z*t_z,
                 uv = u_z*v_z,
                 ttita = t_z*tita_z)][
         , `:=`(dvtita = .derv(vtita, lon*pi/180, cyclical = TRUE),
                 dutita = .derv(utita, lon*pi/180, cyclical = TRUE),
                 dttita = .derv(ttita, lon*pi/180, cyclical = TRUE),
                 p = lev/1000),
            by = .(lev, lat)][
         , `:=`(Flat = p*cos(lat*pi/180)*(v_z^2 - dvtita*2*.omega*a*sin(2*lat*pi/180)),
                 Flon = p*cos(lat*pi/180)*(-uv   + dutita*2*.omega*a*sin(2*lat*pi/180)),
                 Flev   = p*cos(lat*pi/180)/S*coriolis(lat)*(vt  - dttita*2*.omega*a*sin(2*lat*pi/180)))]

    # Undefined global blah blah blah
    tita <- dtp <- S <- tita_z  <-
        t_z <- u_z <- v_z <- vtita <- utita <- vt <- uv <- ttita <- dvtita <-
        dutita <- dttita <- p <- Flat <- Flon <- Flev <- NULL
    data[, .(Flon, Flat, Flev)][]
}
