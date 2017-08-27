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
#' mask <- nceptemperature[lev == 1000, .(lon = lon, lat = lat, land = MakeMask(lon, lat))]
#' nceptemperature <- nceptemperature[mask, on = c("lon", "lat")]
#'
#' # Take the temperature difference between land and ocean
#' diftemp <- nceptemperature[,
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
#' usatemp <- nceptemperature[, usa := MakeMask(lon, lat, mask = "usa")][
#'     , .(air = weighted.mean(air, cos(lat*pi/180))), by = .(usa, lev)][
#'         usa == TRUE]
#'
#' ggplot(usatemp, aes(lev, air)) +
#'     geom_line() +
#'     scale_x_level() +
#'     coord_flip()
#'
#'
#' @export
#' @import maps
#' @import maptools
#' @import sp
MakeMask <- function(lon, lat, mask = "world", wrap = c(0, 360)) {
    # Chek arguments
    valid <- (lat %b% c(-90, 90)) & (lon %b% wrap)

    if (sum(!valid) != 0) warning("Points out of bounds")

    seamask <- maps::map(paste0("maps::", mask), fill = TRUE, col = "transparent",
                         plot = F, wrap = wrap)
    IDs <- sapply(strsplit(seamask$names, ":"), function(x) x[1])
    proj <- sp::CRS("+proj=longlat +datum=WGS84")
    seamask <- maptools::map2SpatialPolygons(seamask, IDs = IDs, proj4string = proj)

    points <- sp::SpatialPoints(data.frame(lon, lat), proj4string = proj)
    land <-  unname(!is.na(sp::over(points, seamask)))
    land[!valid] <- NA
    return(land)
}
