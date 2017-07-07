#' Empirical Orthogonal Function
#'
#' Uses \code{\link[svd]{propack.svd}} to compute Empirical Orthogonal Functions
#' (aka Singular Value Decomposition).
#'
#' @param z numeric vector of variable
#' @param lon longitude (in degrees)
#' @param lon longitude (in degrees)
#' @param date date
#' @param n which singular value to compute
#' @param return should the function return loadings or the field?
#'
#' @return
#' A data.table of loadings or fields.
#'
#' @export
#' @import data.table
EOF <- function(z, lon, lat, date, n = 1, return = c("loadings", "field")) {
    # Calcula EOF del campo z.
    # Entra:
    #   z: campo (en vector)
    #   lon: vector de longitudes
    #   lat: vector de latitudes
    #   date: vector de fechas
    #   n: número de valores principales a calcular
    #   return: devolver el índice o el campo
    # Sale:
    #   un data.table con el campo o el índice.

    field <- data.table(lon, lat, date, z)
    field[, z.w := z*sqrt(cos(lat*pi/180))]    # peso.

    g <- dcast(field, lon + lat ~ date, value.var = "z.w")
    eof <- svd::propack.svd(as.matrix(g[,-(1:2)]), neig = n)

    if (return[1] == "loadings") {
        eof <- as.data.table(eof$v)
        eof <- cbind(eof, data.table(date = colnames(g[, -c(1, 2)])))
    } else {
        eof <- as.data.table(eof$u)
        eof <- cbind(eof, g[, c(1, 2)])
    }

    return(eof)
}
