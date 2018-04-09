#' Derivate a discrete variable using finite differences
#'
#' @param formula a formula indicating dependent and independent variables
#' @param order order of the derivative
#' @param cyclical logical vector of boundary condition for each independent variable
#' @param fill logical indicating whether to fill values at the boundaries
#' with forward and backwards differencing
#' @param data optional data.frame containing the variables
#' @param sphere logical indicating whether to use spherical coordinates
#' (see details)
#' @param a radius for use in spherical coordinates (defaults to Earth's radius)
#'
#'
#' @return
#' If there is one independent variable and one dependent variable, a numeric
#' vector of the same length as the dependent variable.
#' If there are two or more independent variables or two or more dependent variables,
#' a list containing the directional derivatives of each dependent variables.
#'
#' @details
#' Each element of the return vector is an estimation of
#' \eqn{\frac{\partial^n x}{\partial y^{n}}}{d^nx/dy^n} by
#' centered finite differences.
#'
#' If `sphere = TRUE`, then the first two independent variables are
#' assumed to be longitude and latitude (**in that order**) in degrees. Then, a
#' correction is applied to the derivative so that they are in the same units as
#' `a`.
#'
#' Using `fill = TRUE` will degrade the solution near the edges of a non-cyclical
#' bondary. Use with caution.
#'
#' `Laplacian()`, `Divergence()` and `Vorticity()` are convenient wrappers that
#' call `Derivate()` and make the appropriate sums. For `Divergence()` and
#' `Vorticity()`, `formula` must be of the form `vx + vy ~ x + y`
#' (**in that order**).
#'
#' @examples
#' theta <- seq(0, 360, length.out = 20)*pi/180
#' theta <- theta[-1]
#' x <- cos(theta)
#' dx_analytical <- -sin(theta)
#' dx_finitediff <- Derivate(x ~ theta, cyclical = TRUE)[[1]]
#'
#' plot(theta, dx_analytical, type = "l")
#' points(theta, dx_finitediff, col = "red")
#'
#' # Curvature (Laplacian)
#' # Note the different boundary conditions for each dimension
#' variable <- expand.grid(lon = seq(0, 360, by = 3)[-1],
#'                         lat = seq(-90, 90, by = 3))
#' variable$z <- with(variable, cos(lat*pi/180*3) + sin(lon*pi/180*2))
#' variable <- cbind(
#'      variable,
#'      as.data.frame(Derivate(z ~ lon + lat, data = variable,
#'                           cyclical = c(TRUE, FALSE), order = 2)))
#' library(ggplot2)
#' ggplot(variable, aes(lon, lat)) +
#'     geom_contour(aes(z = z)) +
#'     geom_contour(aes(z = z.ddlon + z.ddlat), color = "red")
#'
#' # The same as
#' ggplot(variable, aes(lon, lat)) +
#'     geom_contour(aes(z = z)) +
#'     geom_contour(aes(z = Laplacian(z ~ lon + lat, cyclical = c(TRUE, FALSE))),
#'                  color = "red")
#'
#' @family meteorology functions
#' @import data.table Formula formula.tools
#' @export
Derivate <- function(formula, data = NULL, order = 1, cyclical = FALSE, fill = FALSE,
                     sphere = FALSE, a = 6371000) {
    dep.names <- formula.tools::lhs.vars(formula)
    ind.names <- formula.tools::rhs.vars(formula)
    formula <- Formula::as.Formula(formula)
    data <- as.data.table(eval(quote(model.frame(formula, data  = data))))

    # Order data.
    id.name <- digest::digest(data[, 1])
    data[, (id.name) := 1:.N]
    # data[, id := 1:.N]    # for order.
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
                  cyclical = this.bc, fill = fill)}),
            by = c(ind.names[-v])]
    }
    data <- data[order(data[[id.name]])]

    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        cosvar <- cos(data[, get(ind.names[2])]*pi/180)
        data[, dernames[[1]] := get(dernames[[1]])*(180/pi/(a*cosvar))^order[1]]
        data[, dernames[[2]] := get(dernames[[2]])*(180/pi/a)^order[1]]
    }

    data <- data[, unlist(dernames), with = FALSE]

    return(as.list(data))

}

#' @rdname Derivate
#' @export
Laplacian <- function(formula, data = NULL, cyclical = FALSE, fill = FALSE,
                      sphere = FALSE, a = 6371000) {
    dep.names <- as.character(formula.tools::lhs(formula))
    dep.names <- dep.names[!grepl("+", dep.names, fixed = T)]

    ndep <- length(dep.names)

    lap.name <- paste0(dep.names, ".lap")

    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 2)

    lap <- lapply(seq(ndep), FUN = function(x) {
        Reduce("+", der[seq(x, length(der), by = ndep)])
    })
    names(lap) <- lap.name
    if(length(lap) == 1) {
        return(lap[[1]])
    } else {
        return(lap)
    }
}

#' @export
#' @rdname Derivate
Divergence <- function(formula, data = NULL, cyclical = FALSE, fill = FALSE,
                       sphere = FALSE, a = 6371000) {
    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 1)

    div <- der[[1]] + der[[4]]
    div
}

#' @export
#' @rdname Derivate
Vorticity <- function(formula, data = NULL, cyclical = FALSE, fill = FALSE,
                      sphere = FALSE, a = 6371000) {

    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 1)

    vort <- der[[2]] - der[[3]]
    vort
}


.derv <- function(x, y, order = 1, cyclical = FALSE, fill = FALSE) {
    N <- length(x)
    d <- y[2] - y[1]
    if (order >= 3) {
        dxdy <- .derv(.derv(x, y, order = 2, cyclical = cyclical, fill = fill),
                      y, order = order - 2, cyclical = cyclical, fill = fill)
    } else {
        if (order == 1) {
            dxdy <- (x[c(2:N, 1)] - x[c(N, 1:(N-1))])/(2*d)
        } else if (order == 2) {
            dxdy <- (x[c(2:N, 1)] + x[c(N, 1:(N-1))] - 2*x)/(2*d)^2
        }
        if (!cyclical) {
            if (!fill) {
                dxdy[c(1, N)] <- NA
            }
            if (fill) {
                dxdy[1] <- (-11/6*x[1] + 3*x[2] - 3/2*x[3] + 1/3*x[4])/d
                dxdy[N] <- (11/6*x[N] - 3*x[N-1] + 3/2*x[N-2] - 1/3*x[N-3])/d
            }
        }

    }
    return(dxdy)
}
