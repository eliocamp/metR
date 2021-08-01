#' Impute missing values
#'
#' Imputes missing values via Data Interpolating Empirical Orthogonal Functions
#' (DINEOF).
#'
#' @inheritParams EOF
#' @param max.eof,min.eof maximum and minimum number of singular values used for
#' imputation
#' @param tol tolerance used for determining convergence
#' @param max.iter maximum iterations allowed for the algorithm
#' @param validation number of points to use in cross-validation (defaults to the
#' maximum of 30 or 10% of the non NA points)
#' @param verbose logical indicating whether to print progress
#'
#' @return
#' A vector of imputed values with attributes `eof`, which is the number of
#' singular values used in the final imputation; and `rmse`, which is the Root
#' Mean Square Error estimated from cross-validation.
#'
#' @details
#' Singular values can be computed over matrices so \code{formula} denotes how
#' to build a matrix from the data. It is a formula of the form VAR ~ LEFT | RIGHT
#' (see [Formula::Formula]) in which VAR is the variable whose values will
#' populate the matrix, and LEFT represent the variables used to make the rows
#' and RIGHT, the columns of the matrix.
#' Think it like "VAR *as a function* of LEFT *and* RIGHT".
#'
#' Alternatively, if `value.var` is not `NULL`, it's possible to use the
#' (probably) more familiar [data.table::dcast] formula interface. In that case,
#' `data` must be provided.
#'
#' If `data` is a matrix, the `formula` argument is ignored and the function
#' returns a matrix.
#'
#' @references
#' Beckers, J.-M., Barth, A., and Alvera-Azcárate, A.: DINEOF reconstruction of clouded images including error maps – application to the Sea-Surface Temperature around Corsican Island, Ocean Sci., 2, 183-199, \doi{10.5194/os-2-183-2006}, 2006.
#'
#' @examples
#' library(data.table)
#' data(geopotential)
#' geopotential <- copy(geopotential)
#' geopotential[, gh.t := Anomaly(gh), by = .(lat, lon, month(date))]
#'
#' # Add gaps to field
#' geopotential[, gh.gap := gh.t]
#' set.seed(42)
#' geopotential[sample(1:.N, .N*0.3), gh.gap := NA]
#'
#' max.eof <- 5    # change to a higher value
#' geopotential[, gh.impute := ImputeEOF(gh.gap ~ lat + lon | date, max.eof,
#'                                       verbose = TRUE, max.iter = 2000)]
#'
#' library(ggplot2)
#' ggplot(geopotential[date == date[1]], aes(lon, lat)) +
#'     geom_contour(aes(z = gh.t), color = "black") +
#'     geom_contour(aes(z = gh.impute))
#'
#' # Scatterplot with a sample.
#' na.sample <- geopotential[is.na(gh.gap)][sample(1:.N, .N*0.1)]
#' ggplot(na.sample, aes(gh.t, gh.impute)) +
#'     geom_point()
#'
#' # Estimated RMSE
#' attr(geopotential$gh.impute, "rmse")
#' # Real RMSE
#' geopotential[is.na(gh.gap), sqrt(mean((gh.t - gh.impute)^2))]
#'
#'
#' @export
#' @importFrom stats as.formula
ImputeEOF <- function(formula, max.eof = NULL, data = NULL,
                      min.eof = 1, tol = 1e-2, max.iter = 10000,
                      validation = NULL, verbose = interactive()) {
    checks <- makeAssertCollection()

    assert(
        checkClass(data, "data.frame", null.ok = TRUE),
        checkClass(data, "matrix", null.ok = TRUE))
    assertCount(max.eof, null.ok = TRUE, add = checks)
    assertCount(min.eof, add = checks)
    assertNumber(tol, add = checks)
    assertNumber(max.iter, add = checks)
    assertNumber(validation, null.ok = TRUE, add = checks)
    assertFlag(verbose, add = checks)

    reportAssertions(checks)

    # Build matrix if necessary.
    if (is.null(data) | is.data.frame(data)) {
        assertClass(formula, "formula")

        if (is.null(data)) {
            formula <- Formula::as.Formula(formula)
            data <- data.table::as.data.table(eval(quote(model.frame(formula, data  = data,
                                                                     na.action = NULL))))
        }
        f <- as.character(formula)

        if (length(f) == 1) {  # formula.tool did its thing
            f <- stringr::str_split(f, "~", n = 2)[[1]]
        } else {
            f <- f[-1]
        }
        dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
        dcast.formula <- as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))

        value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

        nas <- sum(is.na(data[[value.var]]))
        if (nas == 0) {
            warningf("%s column has no missing values. Returning unchanged vector.", value.var)
            return(data[[value.var]])
        }
        g <- .tidy2matrix(data, dcast.formula, value.var)
        # id.name <- digest::digest(data[, 1])
        id.name <- "ff19bdd67ff5f59cdce2824074707d20"
        data[, (id.name) := 1:.N]
        id <- c(.tidy2matrix(data, dcast.formula, id.name)$matrix)
        data[, (id.name) := NULL]
        X <- g$matrix
    } else if (is.matrix(data)) {
        X <- data
        nas <- sum(is.na(data))
        if (nas == 0) {
            warningf("'data' has no missing values. Returning unchanged matrix.")
            return(data)
        }
    } else {
        stopf("'data' must be matrix or data frame if not NULL.")
    }
    if (is.null(max.eof)) max.eof <- min(ncol(X), nrow(X))
    gaps <- which(is.na(X))
    # if (length(gaps) == 0) return(X)

    if (is.null(validation)) {
        validation <- max(30, 0.1*length(X[-gaps]))
    }
    set.seed(42)    # let's get reproducible in here.
    validation <- sample(seq_along(X)[!seq_along(X) %in% gaps],
                         validation)

    eofs <- c(0, min.eof:max.eof)
    X.rec <- X
    # First try, imput with mean or something. Rmse is infinite.
    fill <- mean(X[!is.na(X)])
    X.rec[c(gaps, validation)] <- fill
    rmse <- sqrt(mean((X[validation] - X.rec[validation])^2))

    prev <- NULL
    for (i in 2:length(eofs)) {
        # After first guess, impute gaps and validation.
        X.rec <- .ImputeEOF1(X.rec, c(gaps, validation), eofs[i],
                             tol = tol, max.iter = max.iter,
                             verbose = verbose, prev = prev)
        prev <- X.rec$prval
        X.rec <- X.rec$X.rec

        rmse <- c(rmse, sqrt(mean((X[validation] - X.rec[validation])^2)))

        if (verbose == TRUE) {
            message(paste0(sprintf(ngettext(eofs[i],
                                            "With %d eof  - rmse = %.3f",
                                            "With %d eofs - rmse = %.3f", domain = "R-metR"),
                                   eofs[i], rmse[i]), "\r"),  appendLF=FALSE)
        }

        # Break the loop if we are over the minimum eof asked and, either
        # this rmse is greater than the previous one or current rmse is inf.
        if (rmse[i - 1] - rmse[i] < tol) {
            break
        }
    }
    # Select best eof and make proper imputation.
    eof <- eofs[which.min(rmse)]
    X[gaps] <- fill
    X.rec <- .ImputeEOF1(X, gaps, eof, tol = tol, max.iter = max.iter,
                         verbose = verbose)$X.rec

    if (is.data.frame(data)) {
        X.rec <- c(X.rec)[order(id)]
    }

    attr(X.rec, "eof") <- eof
    attr(X.rec, "rmse") <- min(rmse)
    return(X.rec)
}


.ImputeEOF1 <- function(X, X.na, n.eof, tol = 1e-2, max.iter = 10000,
                        verbose = TRUE, prev = NULL) {
    X.rec <- X
    v <- NULL
    rmse <- Inf
    for (i in 2:max.iter) {
        if (requireNamespace("irlba", quietly = TRUE)) {
            set.seed(42)
            prval <- irlba::irlba(X.rec, nv = n.eof, v = prev)
        } else {
            prval <- base::svd(X.rec, nu = n.eof, nv = n.eof)
            prval$d <- prval$d[1:n.eof]
        }
        v <- prval$v
        R <- prval$u%*%diag(prval$d, nrow = n.eof)%*%t(v)
        rmse <- c(rmse, sqrt(mean((R[X.na] - X.rec[X.na])^2)))

        if (rmse[i-1] - rmse[i] > tol) {
            X.rec[X.na] <- R[X.na]
        } else {
            return(list(X.rec = X.rec, prval = prval))
        }
    }
}
