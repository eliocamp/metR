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
#' @param a radius to use in spherical coordinates (defaults to Earth's radius)
#' @param equispaced logical indicating whether points are equispaced or not.
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
#' centred finite differences.
#'
#' If `sphere = TRUE`, then the first two independent variables are
#' assumed to be longitude and latitude (**in that order**) in degrees. Then, a
#' correction is applied to the derivative so that they are in the same units as
#' `a`.
#'
#' Using `fill = TRUE` will degrade the solution near the edges of a non-cyclical
#' boundary. Use with caution.
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
#' @import checkmate
#' @export
Derivate <- function(formula, order = 1, cyclical = FALSE, fill = FALSE,
                     data = NULL, sphere = FALSE, a = 6371000, equispaced = TRUE) {
    checks <- makeAssertCollection()

    assertClass(formula, "formula", add = checks)
    assertDataFrame(data, null.ok = TRUE, add = checks)
    assertCount(order, positive = TRUE, add = checks)
    assertFlag(fill, add = checks)
    assertFlag(sphere, add = checks)
    assertNumber(a, lower = 0, finite = TRUE, add = checks)
    assertLogical(cyclical, any.missing = FALSE, add = checks)

    reportAssertions(checks)

    dep.names <- formula.tools::lhs.vars(formula)
    ind.names <- formula.tools::rhs.vars(formula)

    formula <- Formula::as.Formula(formula)
    data <- data.table::as.data.table(eval(quote(model.frame(formula, data = data,
                                                             na.action = NULL))))

    # id.name <- digest::digest(data[1, 1])
    id.name <- "ff19bdd67ff5f59cdce2824074707d20"
    data.table::set(data, NULL, id.name, 1:nrow(data))
    data.table::setkeyv(data, ind.names[length(ind.names):1])

    if (length(ind.names) > 1) {
        if (length(cyclical) == 1) {
            cyclical <- rep(cyclical, length(ind.names))
        } else if (length(cyclical) < length(ind.names)) {
            stopf("One boundary condition per variable needed.")
        }
    }

    dernames <- lapply(dep.names, FUN = function(x) {
        paste0(x, ".",
               paste0(rep("d", order[1]), collapse = ""),
               ind.names)
    })
    coords <- lapply(ind.names, function(x) unique(data[[x]]))
    # coords <- coords[length(coords):1]

    for (v in seq_along(dep.names)) {
        data.array <- array(data[[dep.names[v]]], dim = unlist(lapply(coords, length)))
        s <- lapply(seq_along(coords), function(x) {
            c(.derv.array(data.array, coords, x, order = order[1],
                          cyclical = cyclical[x], fill = fill,
                          equispaced = equispaced))
        })
        data.table::set(data, NULL, dernames[[v]], s)
    }
    # data <- data[order(data[[id.name]])]
    data.table::setkeyv(data, id.name)

    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        cosvar <- cos(data[, get(ind.names[2])]*pi/180)
        for (var in seq_along(dernames)) {
            data[, dernames[[var]][1] := get(dernames[[var]][1])*(180/pi/(a*cosvar))^order[1]]
            data[, dernames[[var]][2] := get(dernames[[var]][2])*(180/pi/a)^order[1]]
        }
    }

    data <- data[, unlist(dernames), with = FALSE]

    return(as.list(data))
}


# Derivates multidimensional arrays.
.derv.array <- function(X, coords, margin, order = 1, cyclical = FALSE, fill = FALSE,
                        equispaced = TRUE) {
    if (length(dim(X)) == 1) {
        return(.derv(X, coords[[1]], order = order, cyclical = cyclical, fill = fill,
                     equispaced = equispaced))
    }
    dims <- seq(dim(X))
    coord <- coords[[margin]]
    margins <- dims[!(dims %in% margin)]
    f <- apply(X, margins, function(x) .derv(x, coord, order = order,
                                             cyclical = cyclical, fill = fill,
                                             equispaced = equispaced))
    f <- aperm(f, c(margin, margins))
    return(f)
}



#' @rdname Derivate
#' @export
Laplacian <- function(formula, cyclical = FALSE, fill = FALSE,
                      data = NULL, sphere = FALSE, a = 6371000,
                      equispaced = TRUE) {
    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 2,
                    equispaced = equispaced)

    dep.names <- as.character(formula.tools::lhs(formula))
    dep.names <- dep.names[!grepl("+", as.character(dep.names), fixed = TRUE)]
    ndep <- length(dep.names)
    lap.name <- paste0(dep.names, ".lap")

    lap <- lapply(seq(ndep), FUN = function(x) {
        Reduce("+", der[1:ndep + (x-1)*ndep])
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
Divergence <- function(formula, cyclical = FALSE, fill = FALSE,
                       data = NULL, sphere = FALSE, a = 6371000,
                       equispaced = TRUE) {
    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 1,
                    equispaced = equispaced)

    div <- der[[1]] + der[[4]]
    div
}

#' @export
#' @rdname Derivate
Vorticity <- function(formula, cyclical = FALSE, fill = FALSE,
                      data = NULL, sphere = FALSE, a = 6371000,
                      equispaced = TRUE) {
    der <- Derivate(formula = formula, data = data, cyclical = cyclical,
                    sphere = sphere, a = a, order = 1,
                    equispaced = equispaced)

    vort <- -der[[2]] + der[[3]]
    vort
}


.derv <- function(x, y, order = 1, cyclical = FALSE, fill = FALSE, equispaced = TRUE) {
    N <- length(x)

    if (N < order + 1) {
        return(rep(NA, length = length(x)))
    }
    nxt <- function(v) {
        v[c(2:N, 1)]
    }

    prv <- function(v) {
        v[c(N, 1:(N-1))]
    }

    if (cyclical) {
        # Check for equispaced grid
        # even if the user says its equispaced, it might not be.
        # If the user says it's not, then trust them.
        h1 <- diff(y)
        if (equispaced){
            equispaced <- slow_equal(h1)
        }
        if (!equispaced) {
            # TODO
            stopf("Cyclical derivatives on a non-equispaced grid not yet supported.")
        }
        h1 <- rep(h1[1], N)
        h2 <- h1
    } else {
        h2 <- nxt(y) - y
        h1 <- y - prv(y)
    }



    x0 <- prv(x)
    x2 <- nxt(x)

    # Higher order derivatives are taken by succesive differentiation
    if (order >= 2) {
        dxdy <- .derv(x, y, order = 1, cyclical = cyclical, fill = fill,
                     equispaced = equispaced)
        dxdy <- .derv(dxdy, y, order = order - 1, cyclical = cyclical, fill = fill,
                      equispaced = equispaced)

    } else {
        # First order derivative
        if (order == 1) {
            # from http://www.m-hikari.com/ijma/ijma-password-2009/ijma-password17-20-2009/bhadauriaIJMA17-20-2009.pdf
            # Eq 8b (f -> x)
            dxdy <- -(h2/(h1*(h1+h2)))*x0 - (h1 - h2)/(h1*h2)*x + h1/(h2*(h1+h2))*x2
        }
        # else if (order == 2) {
        #     # Eq 11
        #     h2 <- nxt(y) - y
        #     h1 <- y - prv(y)
        #
        #     x0 <- prv(x)
        #     x2 <- nxt(x)
        #     dxdy <- 2*(h2*x0 - (h1+h2)*x + h1*x2)/(h1*h2*(h1+h2))
        # }

        if (!cyclical) {
            if (!fill) {
                dxdy[c(1, N)] <- NA
            }
            if (fill) {
                # Eq 81
                dxdy[1] <- (-(2*h1 + h2)/(h1*(h1 + h2))*x0 +
                    (h1 + h2)/(h1*h2)*x - h1/(h2*(h1 + h2))*x2)[2]

                # Eq 8c
                dxdy[N] <- (h2/(h1*(h1 + h2))*x0 - (h1+h2)/(h1*h2)*x + (2*h2 + h1)/(h2*(h1+h2))*x2)[N-1]

                # dxdy[N] <- (11/6*x[N] - 3*x[N-1] + 3/2*x[N-2] - 1/3*x[N-3])/d
            }
        }
    }
    return(dxdy)
}



# .get_order_dim <- function(data) {
#     setDT(data)
#     data <- copy(data)
#     dims <- colnames(data)
#     dimorder <- vector("character", length(dims))
#     d <- 1
#     while (ncol(data) > 1) {
#         difs <- lapply(data, function(x) diff(x[1:2]))
#         dimorder[d] <- dims[difs != 0]
#         data <- subset(data, get(dimorder[d]) == with(data[1, ], get(dimorder[d])))
#         set(data, NULL, dimorder[d], NULL)
#         d <- d + 1
#     }
#     dimorder[d] <- colnames(data)
#     return(dimorder)
# }
