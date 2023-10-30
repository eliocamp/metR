#' Converts between longitude conventions
#'
#' Converts longitude from [0, 360) to [-180, 180) and vice versa.
#'
#' @param lon numeric vector of longitude
#' @param group optional vector of groups (the same length as longitude)
#' that will be split on the edges (see examples)
#' @param from optionally explicitly say from which convention to convert
#'
#' @return If `group` is missing, a numeric vector the same length of lon.
#' Else, a list with vectors `lon` and `group`.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' \dontshow{data.table::setDTthreads(1)}
#' data(geopotential)
#'
#' ggplot(geopotential[date == date[1]], aes(lon, lat, z = gh)) +
#'     geom_contour(color = "black") +
#'     geom_contour(aes(x = ConvertLongitude(lon)))
#'
#' map <- setDT(map_data("world"))
#' map[, c("lon", "group2") := ConvertLongitude(long, group, from = 180)]
#'
#' ggplot(map, aes(lon, lat, group = group2)) +
#'     geom_path()
#'
#' @export
ConvertLongitude <- function(lon, group = NULL, from = NULL) {
    checks <- makeAssertCollection()

    assertNumeric(lon, add = checks, lower = -180, upper = 360)
    assertVector(group, len = length(lon), null.ok = TRUE, add = checks)
    assert_choice(from, c(180, 360), null.ok = TRUE)

    reportAssertions(checks)

    if (all(is.na(lon))) return(lon)

    m <- min(lon, na.rm = TRUE)
    if (m < -180) stopf("'lon' lower than 180 is not a valid longitude")

    M <- max(lon, na.rm = TRUE)
    if (M > 360) stopf("'lon' greater than 360 is not a valid longitude")

    lon360 <- FALSE
    lon180 <- FALSE

    new.lon <- lon
    if (is.null(from) || from == 180) {
        lon180 <- which(lon < 0)
        new.lon[lon180] <- new.lon[lon180] + 360
    }
    if (is.null(from) || from == 360) {
        lon360 <- which(lon > 180)
        new.lon[lon360] <- new.lon[lon360] - 360
    }

    if (!is.null(group)) {

        group.c <- as.character(group)
        group.c[lon360 | lon180] <- paste0(group.c[lon360 | lon180], "_2")

        if (is.factor(group)) {
            group.c <- factor(group.c)
        }

        return(list(lon = new.lon, group = group.c))
    }

    return(new.lon)
}


