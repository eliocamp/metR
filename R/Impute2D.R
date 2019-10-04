#' Impute missing values by linear or constant interpolation
#'
#' Provides methods for (soft) imputation of missing values.
#'
#' @inheritParams Interpolate
#' @param method "interpolate" for interpolation, a numeric for constant imputation
#' or a function that takes a vector and returns a number (like [mean])
#'
#' @details
#' This is "soft" imputation because the imputed values are not supposed to be
#' representative of the missing data but just filling for algorithms that need
#' complete data (in particular, contouring). The method used if
#' `method = "interpolate"` is to do simple linear interpolation in both the x and y
#' direction and then average the result.
#'
#' This is the imputation method used by [geom_contour_fill()].
#'
#' @export
#' @import data.table
Impute2D <- function(formula, data = NULL, method = "interpolate") {
    checks <- makeAssertCollection()
    assertClass(formula, "formula", add = checks)
    assertDataFrame(data, null.ok = TRUE, add = checks)
    assert(
        checkClass(method, "character"),
        checkClass(method, "numeric"),
        checkClass(method, "function"))

    if (!is.function(method)) {
        assertVector(method, len = 1, add = checks)
    }

    reportAssertions(checks)

    dep.names <- formula.tools::lhs.vars(formula)
    if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")

    ind.names <- formula.tools::rhs.vars(formula)
    if (length(ind.names) > 2) {
        stop("RHS of formula must be of the form x + y")
    }

    formula <- Formula::as.Formula(formula)
    data <- data.table::as.data.table(eval(quote(model.frame(formula, data = data,
                                                 na.action = NULL))))

    if (method == "interpolate") method <- TRUE

    for (var in dep.names) {
        data_sub <- data[, c(ind.names, var), with = FALSE]
        data.table::setnames(data_sub, c(ind.names, var), c("x", "y", "z"))
        set(data, NULL, var, .impute_data(data_sub, na.fill = method, verbose = FALSE)$z)
    }

    return(as.list(data[, dep.names, with = FALSE]))
}


soft_approx <- function(x, y = NULL, xout = x) {
    if (sum(!is.na(y)) < 2) {
        return(rep(NA_real_, length(xout)))
    }
    approx(x, y, xout = xout, rule = 2)$y
}

soft_approx2d <- function(x, y, z) {
    dt <- data.table(x, y, z)
    dt[, z1 := soft_approx(x, z), by = y][, z2 := soft_approx(y, z), by = x]
    dt[, mean(c(z1, z2, z), na.rm = TRUE), by = .(x, y)]$V1
}

.impute_data <- function(data, na.fill = TRUE, verbose = TRUE) {
    nas <- nrow(data[is.na(z)])
    if (nas != 0) {
        if (isTRUE(na.fill)) {
            if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
            data <- copy(data)[, z := soft_approx2d(x, y, z)]
            if (sum(is.na(data[["z"]])) != 0) {
                warning("Linear imputation failed. Try passing a constant number or a function (e.g. `na.fill = mean`).")
            }
        } else if (is.numeric(na.fill)) {
            if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
            data[is.na(z), z := na.fill[1]]
        } else if (is.function(na.fill)) {
            if(isTRUE(verbose)) warning("imputing missing values", call. = FALSE)
            z.fill <- data[is.finite(z), na.fill(z)]
            data[is.na(z), z := z.fill]
        }
    }
    return(data)
}

.impute_data.m <- memoise::memoise(.impute_data)
