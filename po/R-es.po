#' Calculate geostrophic winds
#'
#' Geostrophic wind from a geopotential height field.
#'
#' @param gh geopotential height
#' @param lon longitude in degrees
#' @param lat latitude in degrees
#' @param cyclical boundary condition for longitude (see details)
#' @param g acceleration of gravity
#' @param a Earth's radius
#'
#' @details
#' If `cyclical = "guess"` (the default) the function will try to guess if `lon`
#' covers the whole globe and set cyclical conditions accordingly. For more
#' predictable results, set the boundary condition explicitly.
#'
#' @return
#' A named list with vectors for the zonal and meridional component of geostrophic
#' wind.
#'
#' @examples
#' data(geopotential)
#' geopotential <- data.table::copy(geopotential)
#' geopotential[date == date[1], c("u", "v") := GeostrophicWind(gh, lon, lat)]
#' library(ggplot2)
#' ggplot(geopotential[date == date[1]], aes(lon, lat)) +
#'     geom_contour(aes(z = gh)) +
#'     geom_vector(aes(dx = u, dy = v), skip = 2) +
#'     scale_mag()
#'
#' @export
#' @family meteorology functions
GeostrophicWind <- function(gh, lon, lat, cyclical = "guess", g = 9.81, a = 6371000) {
    checks <- makeAssertCollection()
    assertNumeric(gh, add = checks)
    assertNumeric(lon, add = checks)
    assertNumeric(lat, add = checks)
    lengths <- c(gh = length(gh), lon = length(lon), lat = length(lat))
    assertSameLength(lengths, add = checks)
    assertNumber(g, finite = TRUE, add = checks)
    assertNumber(a, finite = TRUE, add = checks)
    reportAssertions(checks)

    if (cyclical == "guess") {
        cyclical <- FALSE
        rlon <- diff(range(lon)) + ggplot2::resolution(lon, zero = FALSE)
        if (rlon == 360) cyclical <- TRUE
    }

    gh.d <- Derivate(gh ~ lon + lat, sphere = TRUE, cyclical = c(cyclical, FALSE),
                     a = a)
    u <- -g*gh.d$gh.dlat/coriolis(lat)
    v <- g*gh.d$gh.dlon/coriolis(lat)
    return(list(ug = u, vg = v))
}



