#' Bilinear interpolation
#'
#' Uses [fields::interp.surface] to interpolate values defined in a bidimensional
#' grid with bilinear interpolation.
#'
#' @param formula a formula indicating dependent and independent variables (see Details)
#' @param x.out,y.out x and y values where to interpolate (see Details)
#' @param data optional data.frame with the data
#' @param grid logical indicating if x.out and y.out define a regular grid.
#' @param path a logical or character indicating if the x.out and y.out define a
#' path. If character, it will be the name of the column returning the order of
#' said path.
#'
#' @details
#' `formula` must be of the form VAR1 | VAR2 ~ X + Y where VAR1, VAR2, etc...
#' are the names of the variables to interpolate and X and Y the names of the
#' x and y values, respectively. It is also possible to pass only values of x,
#' in which case, regular linear interpolation is performed and y.out, if exists,
#' is ignored with a warning.
#'
#' If `grid = TRUE`, `x.out` and `y.out` must define the values of a regular
#' grid. If `grid = FALSE`, they define the locations where to interpolate.
#' Both `grid` and `path` cannot be set to `TRUE` and the value of `path` takes
#' precedence.
#'
#' `x.out` can be a list, in which case, the first two elements will be interpreted
#' as the x and y values where to interpolate and it can also have a `path` element
#' that will be used in place of the `path` argument. This helps when creating a
#' path with [as.path] (see Examples)
#'
#' @return
#' A data.frame with interpolated values and locations
#'
#' @examples
#' library(data.table)
#' data(geopotential)
#' geopotential <- geopotential[date == date[1]]
#' # new grid
#' x.out <- seq(0, 360, by = 10)
#' y.out <- seq(-90, 0, by = 10)
#'
#' # Interpolate values to a new grid
#' interpolated <- geopotential[, Interpolate(gh ~ lon + lat, x.out, y.out)]
#'
#' # Add values to an existing grid
#' geopotential[, gh.new := Interpolate(gh ~ lon + lat, lon, lat,
#'                                      data = interpolated, grid = FALSE)$gh]
#'
#' # Interpolate multiple values
#' geopotential[, c("u", "v") := GeostrophicWind(gh, lon, lat)]
#' interpolated <- geopotential[, Interpolate(u | v ~ lon + lat, x.out, y.out)]
#'
#' # Interpolate values following a path
#' lats <- c(-34, -54, -30)   # start and end latitudes
#' lons <- c(302, 290, 180)   # start and end longituded
#' path <- geopotential[, Interpolate(gh ~ lon + lat, as.path(lons, lats))]
#'
#' @export
#' @importFrom fields interp.surface
#' @import data.table Formula formula.tools
Interpolate <- function(formula, x.out, y.out, data = NULL, grid = TRUE, path = FALSE) {
    checks <- makeAssertCollection()

    assertClass(formula, "formula", add = checks)
    assertDataFrame(data, null.ok = TRUE, add = checks)
    assertFlag(grid, add = checks)
    assertFlag(path, add = checks)

    reportAssertions(checks)

    if (is.list(x.out) == TRUE) {
        path <- x.out$path %||% path
        y.out <- x.out[[2]]
        x.out <- x.out[[1]]
    }

    index <- NULL
    if (!isFALSE(path) & !is.null(path)) {
        if (isTRUE(path)) {
            path <- ".order"
        } else if (!is.character(path)) {
            stop("`order` must be logical or character")
        }
        grid <- FALSE

        index <- seq_along(x.out)
    }

    dep.names <- formula.tools::lhs.vars(formula)
    if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")

    ind.names <- formula.tools::rhs.vars(formula)
    if (length(ind.names) > 2) {
        stop("RHS of formula must be of the form x + y")
    }

    formula <- Formula::as.Formula(formula)
    data <- as.data.table(eval(quote(model.frame(formula, data = data,
                                                 na.action = NULL))))
    if (!.is.reggrid(data, ind.names)) {
        stop("Interpolate need a unique value for each x and y")
    }

    # Accomodate 1D interpolation
    if (length(ind.names) == 1) {
        if (hasArg(y.out)) warning("Only 1 dimension in formula. Ignoring y.out.")

        loc <- data.table(x.out = x.out)
        colnames(loc) <- ind.names
        alloc.col(loc, ncol(data))

        for (v in seq_along(dep.names)) {
            value.var <- dep.names[v]
            set(loc, NULL, value.var, stats::approx(data[[ind.names]],
                                                    data[[value.var]],
                                                    xout = x.out)$y)
        }
        return(loc)
    }

    if (grid == TRUE) {
        if (length(unique(x.out)) != length(x.out)) {
            stop('duplicate values on x.out. If x.out is a vector of locations, use grid = FALSE')
        }
        if (length(unique(y.out)) != length(y.out)) {
            stop('duplicate values on y.out. If y.out is a vector of locations, use grid = FALSE')
        }
        loc <- setDT(expand.grid(x.out = x.out, y.out = y.out))
    } else if (grid == FALSE) {
        if (length(x.out) != length(y.out)) {
            stop('x.out is not of the same length as y.out.
                 If x.out and y.out define unique points on a regular grid, use grid = TRUE')
        }
        loc <- data.table(x.out, y.out)
        if (!is.null(path) & !isFALSE(path)) {
            set(loc, NULL, path, index)
        }
    } else {
        stop('wrong mode, choose either "grid" or "locations"')
    }

    colnames(loc)[seq_along(ind.names)] <- ind.names
    alloc.col(loc, ncol(data))

    dcast.formula <- as.formula(paste0(ind.names, collapse = " ~ "))

    for (v in seq_along(dep.names)) {
        value.var <- dep.names[v]
        data2 <- .tidy2matrix(data, dcast.formula, value.var = value.var)
        data2 <- list(x = data2$rowdims[[1]],
                      y = data2$coldims[[1]],
                      z = data2$matrix)
        set(loc, NULL, value.var, fields::interp.surface(data2, as.matrix(loc)))
    }

    return(as.data.table(loc))
}


#' Interpolates between locations
#'
#' This is a helper function to quickly make an interpolated list of locations
#' between a number of locations
#'
#' @param x,y numeric vectors of x and y locations. If one of them is of length 1,
#' if will be recycled.
#' @param path either `TRUE` of a character vector with the name of the path.
#' @param n number of points to interpolate to
#'
#' @return
#'
#' A list of components `x` and `y` with the list of locations and the `path`
#' arguments
#'
#' @details
#'
#' This function is mostly useful when combined with [Interpolate]
#'
#' @seealso Interpolate
#'
#' @export
#' @importFrom stats approx
as.path <- function(x, y, n = 10, path = TRUE) {
    if (length(x) < 2 & length(y) < 2) {
        stop("either `xs` or `ys` must be of length greater than 1")
    }

    if (!is.numeric(x)) {
        stop("`x` must be a numeric vector")
    }

    if (!is.numeric(y)) {
        stop("`y` must be a numeric vector")
    }

    if (length(x) == 1) {
        x <- rep(x, length(y))
    } else if (length(y) == 1) {
        y <- rep(y, length(x))
    }

    points <- seq_len(length(x))
    index <- seq(min(points), max(points), length.out = n)
    index <- sort(unique(c(index, points)))
    xs_interpolate <- approx(points, x, index)$y
    ys_interpolate <- approx(points, y, index)$y

    list(x = xs_interpolate, y = ys_interpolate,
                path = path)
}
