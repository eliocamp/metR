#' Mask
#'
#' Creates a  mask
#'
#' @param lon a vector of longitudes in degrees in 0-360 format
#' @param lat a vector of latitudes in degrees
#' @param mask the name of the dataset (that will be load with
#' \code{\link[maps]{map}}) for creating the mask
#' @param wrap the longitude range to be used for a global mask
#'
#' @return
#' A logical vector of the same length as lat and lon where \code{TRUE} means
#' that the point is inside one of the polygons making up the map. For a global
#' map (the default), this means that the point is over land.
#'
#' @examples
#'
#' # Make a sea-land mask
#' mask <- temperature[lev == 1000, .(lon = lon, lat = lat, land = MaskLand(lon, lat))]
#' temperature <- temperature[mask, on = c("lon", "lat")]
#'
#' # Take the temperature difference between land and ocean
#' diftemp <- temperature[,
#'           .(tempdif = mean(air[land == TRUE]) - mean(air[land == FALSE])),
#'            by = .(lat, lev)]
#' library(ggplot2)
#' ggplot(diftemp, aes(lat, lev)) +
#'     geom_contour(aes(z = tempdif, color = ..level..)) +
#'     scale_y_level() +
#'     scale_x_latitude() +
#'     scale_color_divergent()
#'
#' # Mean temperature in the USA
#' usatemp <- temperature[, usa := MaskLand(lon, lat, mask = "usa")][
#'     , .(air = weighted.mean(air, cos(lat*pi/180))), by = .(usa, lev)][
#'         usa == TRUE]
#'
#' ggplot(usatemp, aes(lev, air)) +
#'     geom_line() +
#'     scale_x_level() +
#'     coord_flip()
#'
#' @export
MaskLand <- function(lon, lat, mask = "world", wrap = c(0, 360)) {
    checks <- makeAssertCollection()
    assertSameLength(c(lon = length(lon), lat = length(lat)), .var.name = "lon and lat",
                     add = checks)
    assertCharacter(mask, len = 1, add = checks)
    assertNumeric(wrap, len = 2, add = checks)
    reportAssertions(checks)

    check_packages(c("maps", "maptools"), "MaskLand")

    seamask <- maps::map(paste0("maps::", mask), fill = TRUE, col = "transparent",
                         plot = FALSE, wrap = wrap)
    IDs <- vapply(strsplit(seamask$names, ":"), function(x) x[1], "")
    proj <- sp::CRS("+proj=longlat +datum=WGS84")
    seamask <- maptools::map2SpatialPolygons(seamask, IDs = IDs, proj4string = proj)

    field <- data.table::data.table(lon, lat)
    field.unique <- unique(field)

    points <- sp::SpatialPoints(field.unique, proj4string = proj)
    field.unique[, land := unname(!is.na(sp::over(points, seamask)))]
    field.unique[!((lat %between% c(-90, 90)) & (lon %between% wrap)), land := NA]

    field <- field.unique[, .(lon, lat, land)][field, on = c("lon", "lat")]

    return(field$land)
}
