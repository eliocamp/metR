#' Bilinear inteprolation
#'
#' Uses [fields::interp.surface] to interpolate values defined in a bidimensional
#' grid with bilinear interpolation.
#'
#' @param formula a formula indicating dependent and independent variables (see Details)
#' @param x.out,y.out x and y values where to interpolate (see Details)
#' @param data optional data.frame with the data
#' @param grid logical indicating if x.out and y.out define a regular grid.
#'
#'
#' @details
#' `formula` must be of the form VAR1 | VAR2 ~ X + Y where VAR1, VAR2, etc...
#' are the names of the variables to interpolate and X and Y the names of the
#' x and y values, respectively.
#'
#' If `grid = TRUE`, `x.out` and `y.out` must define the values of a regular
#' grid. If `grid = FALSE`, they define the locations where to interpolate.
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
#' @export
#' @importFrom fields interp.surface
#' @import data.table Formula formula.tools
Interpolate <- function(formula, x.out, y.out, data = NULL, grid = TRUE) {
    dep.names <- formula.tools::lhs.vars(formula)
    ind.names <- formula.tools::rhs.vars(formula)
    formula <- Formula::as.Formula(formula)
    data <- as.data.table(eval(quote(model.frame(formula, data = data,
                                                 na.action = NULL))))

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
    } else {
        stop('wrong mode, choose either "grid" or "locations"')
    }
    colnames(loc) <- ind.names
    dcast.formula <- as.formula(paste0(ind.names, collapse = " ~ "))

    for (v in seq_along(dep.names)) {
        value.var <- dep.names[v]
        data2 <- .tidy2matrix(data, dcast.formula, value.var = value.var)
        data2 <- list(x = data2$rowdims[[1]],
                      y = data2$coldims[[1]],
                      z = data2$matrix)
        loc[, (value.var) := fields::interp.surface(data2, as.matrix(loc))]
    }

    return(as.data.table(loc))
}






