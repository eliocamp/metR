#' Derivate a discrete variable using finite differences
#'
#' @param formula a formula indicating dependent and independent variables
#' @param order order of the derivative
#' @param cyclical logical vector of boundary condition for each independent variable
#' @param data optional data.frame containing the variables
#' @param sphere logical indicating whether to use spherical coordinates
#' (see details)
#' @param a radius for use in spherical coordinates (defaults to Earth's raduis)
#'
#'
#' @return
#' If there is one independent variable, a numeric vector of the same length as
#' the dependent variable.
#' If there is two or more independent variables, a list containing the
#' directional derivatives of the dependent variables.
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
#' dx_finitediff <- Derivate(x ~ theta, cyclical = TRUE)
#'
#' plot(theta, dx_analytical, type = "l")
#' points(theta, dx_finitediff, col = "red")
#'
#' # Curvature (Laplacian)
#' # Note the different boundary conditions for each dimension
#' variable <- expand.grid(lon = seq(0, 360, by = 3)[-1],
#'                         lat = seq(-90, 90, by = 3))
#' variable$z <- with(variable, cos(lat*pi/180*3) + sin(lon*pi/180*2))
#' variable <- cbind(variable,
#'                   as.data.frame(Derivate(z ~ lon + lat, data = variable,
#'                                         cyclical = c(TRUE, FALSE), order = 2)))
#' library(ggplot2)
#' ggplot(variable, aes(lon, lat)) +
#'     geom_contour(aes(z = z)) +
#'     geom_contour(aes(z = z.ddlon + z.ddlat), color = "red")
#'
#' @family meteorology functions
#' @seealso \code{\link{DerivatePhysical}}
#' @import data.table
#' @export
Derivate <- function(formula, data = NULL, order = c(1, 2), cyclical = FALSE,
                     sphere = FALSE, a = 6371000) {
    # Build data.frame
    f <- as.character(formula)
    ind.var <- eval(model.frame(formula = paste0("~", f[[3]]), data = data),
                  parent.frame())
    dep.var <- eval(model.frame(formula = paste0("~", f[[2]]), data = data),
                     parent.frame())
    data <- as.data.table(cbind(dep.var, ind.var))

    dep.names <- colnames(data)[seq(ncol(dep.var))]
    ind.names <- colnames(data)[-seq(ncol(dep.var))]

    # Order data.
    data[, id := 1:.N]    # for order.
    setkeyv(data, ind.names)

    if (length(ind.names) > 1) {
        if (length(cyclical) == 1) {
            cyclical <- rep(cyclical, length(ind.names))
        } else if (length(cyclical) < length(ind.names)) {
            stop("One boundary condition per variable needed.")
        }
    }

    dernames <- lapply(ind.names, FUN = function(x) {
        paste0(dep.names, ".",
               paste0(rep("d", order[1]), collapse = ""),
               x)
    })

    for (v in seq_along(ind.names)) {
        this.var <- ind.names[v]
        this.bc <- cyclical[v]

        data[, dernames[[v]] := lapply(seq(dernames[[1]]), function(x) {
                        .derv(.SD[[x]], .SD[[this.var]], order = order[1],
                              cyclical = this.bc)}),
            by = c(ind.names[-v])]
    }
    data <- data[order(id)]

    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        cosvar <- cos(data[, get(ind.names[2])]*pi/180)
        data[, dernames[[1]] := get(dernames[[1]])*(180/pi/(a*cosvar))^order[1]]
        data[, dernames[[2]] := get(dernames[[2]])*(180/pi/a)^order[1]]
    }

    data <- data[, unlist(dernames), with = FALSE]

    if (length(unlist(dernames)) == 1) {
        return(data[[1]])
    } else {
        return(as.list(data))
    }
}


.derv <- function(x, y, order = c(1, 2), cyclical = FALSE) {
    N <- length(x)

    d <- y[2] - y[1]

    if (order[1] == 1) {
        dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)

    } else if (order[1] == 2) {
        dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/d^2
    }
    if (!cyclical) {
        dxdy[c(1, N)] <- NA
    }

    return(dxdy)
}

#' @rdname Derivate
#' @export
Laplacian <- function(formula, data = NULL, cyclical = FALSE,
                      sphere = FALSE, a = 6371000) {
    f <- as.character(formula)
    dep.names <- strsplit(f[[2]], "+", fixed = TRUE)[[1]]
    dep.names <- sub(" ", "", dep.names)
    ind.names <- strsplit(f[[3]], "+", fixed = TRUE)[[1]]
    ndep <- length(dep.names)
    nind <- length(ind.names)
    lap.name <- paste0(dep.names, ".lap")

    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 2)

    lap <- lapply(seq(ndep), FUN = function(x) {
        Reduce("+", der[seq(x, length(der), by = ndep)])
    })
    names(lap) <- lap.name
    lap
}


#' Zonal or meridional derivative
#'
#' Derivates a variable in the zonal or meridional direction taking into account
#' the size of the Earth.
#'
#' @inheritParams Derivate
#' @param variable numeric vector of discrete values
#' @param lon numeric vector of longitude (in degrees)
#' @param lat numeric vector of latitude (in degrees)
#'
#' @details
#' This function is useful when using gridded data in a longitude-latitude
#' regular grid and one needs to compute zonal or meridional derivatives. If
#' \code{lon} is of length 1, it computes the meridional derivative (\eqn{d/dy})
#' at the given longitude. If \code{lat} is of length 1, then it computes
#' the zonal derivative (\eqn{d/dx}) at the given latitude.
#'
#' @return
#' A numeric vector of the same length as \code{variable}.
#'
#' @examples
#' library(data.table)
#' temp.derv <- nceptemperature[lev == 500,
#'                               .(lat = lat, air.dy = DerivatePhysical(air,
#'                                                       lon = lon, lat, cyclical = FALSE)),
#'                 by = .(lev, lon)]
#' library(ggplot2)
#' ggplot(temp.derv, aes(lon, lat)) +
#'     geom_contour(aes(z = air.dy, color = ..level..)) +
#'     scale_color_divergent()
#'
#' @seealso \code{\link{Derivate}}
#' @family meteorology functions
#' @export
DerivatePhysical <- function(variable, lon, lat, order = c(1, 2),
                             cyclical = FALSE, a = 6731000) {
    if (length(lon) == 1) {
        dv <- .derv(variable, lat*pi/180, order = order[1],
                    cyclical = cyclical)/a^order[1]
    } else {
        dv <- Derivate(variable, lon*pi/180, order = order[1],
                       cyclical = cyclical)/(a*cos(lat*pi/180))^order[1]
    }
    return(dv)
}
