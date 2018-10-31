#' Effects of the Earth's rotation
#'
#' Coriolis and beta parameters by latitude.
#'
#' @param lat latitude in degrees
#' @param a radius of the earth
#'
#' @details
#' All functions use the correct sidereal day (24hs 56mins 4.091s) instead of
#' the incorrect solar day (24hs) for 0.3\% more precision and 1000\% more
#' pedantry.
#'
#' @export
coriolis <- function(lat) {
    assertNumeric(lat, -90, 90)
    2*.omega*sin(lat*pi/180)
}


#' @export
#' @rdname coriolis
f <- coriolis

#' @export
#' @rdname  coriolis
coriolis.dy <- function(lat, a = 6371000) {
    assertNumeric(lat, -90, 90)
    2*.omega*cos(lat*pi/180)/a
}

#' @export
#' @rdname coriolis
f.dy <- coriolis.dy

.omega <- 2*pi/(23*3600 + 56*60 + 4.091)
