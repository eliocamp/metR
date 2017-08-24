#' Derivate a discrete variable using finite differences
#'
#' @param formula a formula indicating dependent and independend variables
#' @param order order of the derivative
#' @param bc boundary conditions for each independend variable
#' @param data optional data.frame containing the variables
#' @param sphere logical indicating wheter to use spherical coordinates
#' (see details)
#' @param a radius or the Earth in kilometers for use in spherical coordinates
#'
#'
#' @return
#' If there is one independent variable, a numeric vector of the same length as
#' the dependent variable.
#' If there is two or more independent variables, a list containing the
#' directional derivatives of the dependent variable.
#'
#' @details
#' Each element of the return vector is an estimation of \eqn{dx/dy} (or
#' \eqn{d^2x/dy^2} if \code{order} = 2) by centerd finite differences. The first and
#' last elements will be \code{NAs} unless cyclical boundary conditions are set
#' in \code{bc}.
#'
#' If \code{sphere} is \code{TRUE}, then the first two independent variables are
#' assumed to be longitude and latitude (in that order) in degrees. Then, a
#' correction is applied to the derivative so that they are in units of distance.
#'
#' @examples
#' theta <- seq(0, 360, length.out = 20)*pi/180
#' theta <- theta[-1]
#' x <- cos(theta)
#' dx_analytical <- -sin(theta)
#' dx_finitediff <- Derivate(x ~ theta, bc = "cyclic")
#'
#' plot(theta, dx_analytical, type = "l")
#' points(theta, dx_finitediff, col = "red")
#'
#' # Curvature (Laplacian)
#' # Note the different boundary conditions for each variable
#' variable <- expand.grid(lon = seq(0, 360, by = 3)[-1],
#'                         lat = seq(-90, 90, by = 3))
#' variable$z <- with(variable, cos(lat*pi/180*3) + sin(lon*pi/180*2))
#' variable <- cbind(variable,
#'                   as.data.frame(Derivate(z ~ lon + lat, data = variable,
#'                                         bc = c("cyclic", "none"), order = 2)))
#' library(ggplot2)
#' ggplot(variable, aes(lon, lat)) +
#'     geom_contour(aes(z = z)) +
#'      geom_contour(aes(z = z.ddlon + z.ddlat), color = "red")
#'
#' @family meteorology functions
#' @seealso \code{\link{DerivatePhysical}}
#' @export
Derivate <- function(formula, order = c(1, 2), bc = "none", data = NULL,
                      sphere = FALSE, a = 6731) {
    # Build dataframe
    mf <- match.call(expand.dots = F)
    mf[[1]] <- quote(model.frame)
    m <- match(c("formula", "data"), names(mf))
    mf <- mf[c(1L, m)]
    # mff <<- mf
    data <- as.data.table(eval(mf, parent.frame()))
    # f <<- copy(data)
    dep.var <- colnames(data)[1]
    ind.var <- colnames(data)[-1]
    data[, id := 1:.N]    # for order.
    setkeyv(data, ind.var)

    if (length(ind.var) > 1) {
        if (length(bc) == 1) {
            bc <- rep(bc, length(ind.var))
        } else if(length(bc) < length(ind.var)) {
            stop("One boundary condition per variable needed.")
        }
    }

    dernames <- paste0(dep.var, ".", paste0(rep("d", order[1]), collapse = ""), ind.var)
    for (v in seq_along(ind.var)) {
        temp <- data[, .(id,
                         .derv(get(dep.var), get(ind.var[v]),
                                                   order = order[1], bc = bc[v])),
                                 by = c(ind.var[-v])]
        setnames(temp, "V2", dernames[v])
        data <- data[temp, on = "id"]
    }
    data <- data[order(id)]
    # r <<- copy(data)

    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        a <- a*1000
        set(data, j = dernames[1],
            value = data[, get(dernames[1])]*(180/pi/(a*cos(data[, get(ind.var[2])]*pi/180)))^order[1])
        set(data, j = dernames[2],
            value = data[, get(dernames[2])]*(180/pi/a)^order[1])
    }

    data <- data[, dernames, with = F]

    if (length(ind.var) == 1) {
        return(data[[1]])
    } else {
        return(as.list(data))
    }
}


.derv <- function(x, y, order = c(1, 2), bc = c("cyclic", "none")) {
    N <- length(x)

    d <- y[2] - y[1]

    if (order[1] == 1) {
        dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)

    } else if (order[1] == 2) {
        dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/d^2
    }
    if (bc[1] != "cyclic") {
        dxdy[c(1, N)] <- NA
    }

    return(dxdy)
}

#' Zonal or meridional derivative
#'
#' Derivates a variable in the zonal or meridional direction taking into account
#' the size of the Earth.
#'
#' @param variable numeric vector of discrete values
#' @param lon numeric vector of longitude (in degrees)
#' @param lat numeric vector of latitude (in degrees)
#' @param order order of the derivative
#' @param bc boundary conditions
#' @param a the radius of the earth in kilometers
#'
#' @details
#' This function is usefull when using gridded data in a longitude-latitude
#' regular grid and one needs to compute zonal or meridional derivatives. If
#' \code{lon} is of length 1, it computes the meridional derivative (\eqn{d/dy})
#' at the given longitude. If \code{lat} is of length 1, then it computes
#' the zonal derivative (\eqn{d/dx}) at the given latitude.
#'
#' @return
#' A numeric vector of the same length as \code{variable}.
#'
#' @examples
#' nceptemperature[, air.dy := DerivatePhysical(air, lon = lon, lat, bc = "none"),
#'                 by = .(lev, lon)]
#'
#' ggplot(nceptemperature[lev == 500], aes(lon, lat)) +
#'     geom_tile(aes(fill = air)) +
#'     geom_contour(aes(z = air.dy, color = ..level..)) +
#'     scale_color_divergent()
#'
#' @seealso \code{\link{Derivate}}
#' @family meteorology functions
#' @export
DerivatePhysical <- function(variable, lon, lat, order = c(1, 2),
                             bc = c("cyclic", "none"), a = 6731) {
    a <- a*1000
    if (length(lon) == 1) {
        # dv/dy
        dv = .derv(variable, lat*pi/180, order = order[1],
                      bc = bc[1])/a^order[1]
    } else {
        dv = Derivate(variable, lon*pi/180, order = order[1],
                      bc = bc[1])/(a*cos(lat*pi/180))^order[1]
    }
    return(dv)
}



