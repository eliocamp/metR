#' Converts between longitude conventions
#'
#' Converts longitude from [0, 360) to [-180, 180) and vice versa.
#'
#' @param lon numeric vector of longitude
#' @param from numeric vector representing the convention of the input vector
#'
#' @return A numeric vector the same length of lon.
#'
#' @examples
#' library(ggplot2)
#' ggplot(aao[date == date[1]], aes(lon, lat, z = gh)) +
#'     geom_contour(color = "black") +
#'     geom_contour(aes(x = ConvertLongitude(lon, 360)))
#'
#' @export
ConvertLongitude <- function(lon, from = c(360, 180)) {
    # Pasa la longitud entre convenciones.
    # Entra:
    #   lon: un vector de longitudes
    #   from: la convención desde la cual se convierte
    #   (360 = 0:360, 180 = -180:180)
    # Sale:
    #   un vector con la longitud convertida a la otra convención.
    # Ojo que no hay ningún chequeo de los argumentos. Si se pasa un vector
    # en convención 0:360 y se le dice que está en -180:180, lo "convierte"
    # igual y tira cualquier batata.
    if (from[1] == 360) {
        lon <- ifelse(lon <= 180, lon, lon - 360)
    } else if (from[1] == 180) {
        lon <- ifelse(lon < 0, lon + 360, lon)
    }
    return(lon)
}
