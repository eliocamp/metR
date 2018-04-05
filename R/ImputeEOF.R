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
#' @param validation number of points to use in crossvalidation (defaults to the
#' maximum of 30 or 10\% of the non NA points)
#' @param verbose logical indicating whether to print progress
#'
#' @return
#' A vector of inputed values with attributes `eof`, which is the number of
#' singular values used in the final inputation; and `rmse`, which is the Root
#' Mean Square Error estimated from crossvalidation.
#'
#' @details
#' Singular values can be computed over matrices so `formula` denotes how
#' to build a matrix from the data. It is a formula of the form VAR | LHS ~ RHS
#' in which VAR is the variable whose values will populate the matrix, and LHS
#' represent the variables used to make the rows and RHS, the columns  of the matrix
#' (the form LHS ~ RHS | VAR is also valid).
#'
#' If `data` is a matrix, the `formula` argument is ignored and the function
#' returns a matrix.
#'
#' @references
#' Beckers, J.-M., Barth, A., and Alvera-Azcárate, A.: DINEOF reconstruction of clouded images including error maps – application to the Sea-Surface Temperature around Corsican Island, Ocean Sci., 2, 183-199, https://doi.org/10.5194/os-2-183-2006, 2006.
#'
#' @examples
#' library(data.table)
#' data(geopotential)
#' geopotential <- copy(geopotential)
#' geopotential[, gh.t := Anomaly(gh), by = .(lat, lon, month(date))]
#' # geopotential <- geopotential[date == date[1]]
#'
#' # Add gaps to field
#' geopotential[, gh.gap := gh.t]
#' set.seed(42)
#' geopotential[sample(1:.N, .N*0.3), gh.gap := NA]
#'
#' max.eof <- 10
#' geopotential[, gh.impute := ImputeEOF(gh.gap | lat + lon ~ date, max.eof = max.eof,
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
#' @import data.table
#' @export
#' @importFrom stats as.formula
ImputeEOF <- function(formula, data = NULL, max.eof = NULL,
                      min.eof = 1, tol = 1e-2, max.iter = 10000,
                      validation = NULL, verbose = interactive()) {
    # Build matrix if necessary.

    if (is.null(data) | is.data.frame(data)) {
        f <- as.character(formula)
        f <- stringr::str_split(f,"\\|", n = 2)[[1]]
        dcast.formula <- as.formula(stringr::str_squish(f[stringr::str_detect(f, "~")]))
        value.var <- stringr::str_squish(f[!stringr::str_detect(f, "~")])

        formula <- Formula::as.Formula(formula)
        data <- as.data.table(eval(quote(model.frame(formula, data  = data, na.action = NULL))))

        nas <- sum(is.na(data[[value.var]]))
        if (nas == 0) {
            warning("data has no missing values")
            return(data[[value.var]])
        }
        g <- .tidy2matrix(data, dcast.formula, value.var)
        id.name <- digest::digest(data[, 1])
        data[, (id.name) := 1:.N]
        id <- c(.tidy2matrix(data, dcast.formula, id.name)$matrix)
        X <- g$matrix
    } else if (is.matrix(data)) {
        X <- data
        nas <- sum(is.na(data))
        if (nas == 0) {
            warning("data has no missing values")
            return(data)
        }
    } else {
        stop("data argument must be matrix or data frame")
    }
    if (is.null(max.eof)) max.eof <- min(ncol(X), nrow(X))
    gaps <- which(is.na(X))
    # if (length(gaps) == 0) return(X)

    if (is.null(validation)) {
        validation <- max(30, 0.1*length(X[-gaps]))
    }
    set.seed(42)    # let's get reproducible in here.
    validation <- sample((1:length(X))[!1:length(X) %in% gaps],
                         validation)

    eofs <- c(0, min.eof:max.eof)
    X.rec <- X
    # First try, imput with mean or something. Rmse is infinite.
    fill <- mean(X[!is.na(X)])
    X.rec[c(gaps, validation)] <- fill
    rmse <- sqrt(mean((X[validation] - X.rec[validation])^2))

    for (i in 2:length(eofs)) {
        # After first guess, impute gaps and validation.
        X.rec <- .ImputeEOF1(X.rec, c(gaps, validation), eofs[i],
                                  tol = tol, max.iter = max.iter,
                                  verbose = verbose)

        rmse <- c(rmse, sqrt(mean((X[validation] - X.rec[validation])^2)))

        if (verbose == TRUE) {
            cat("\r", "With", eofs[i], "eof - rmse = ", rmse[i])
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
                         verbose = verbose)

    if (is.data.frame(data)) {
        X.rec <- c(X.rec)[order(id)]
        # X.rec <- melt(X.rec, value.name = value.var)[value.var]
        # id <- melt(id, value.name = "id")["id"]
        # X.rec[id]
        # dimnames(X.rec) <- list(unlist(g$rowdims), unlist(g$coldims))
        # names(dimnames(X.rec)) <- list(names(g$rowdims), names(g$coldims))

    }

    attr(X.rec, "eof") <- eof
    attr(X.rec, "rmse") <- min(rmse)
    return(X.rec)
}


.ImputeEOF1 <- function(X, X.na, n.eof, tol = 1e-2, max.iter = 10000, verbose = TRUE) {
    X.rec <- X
    v <- NULL
    rmse <- Inf
    prval <- NULL
    for (i in 2:max.iter) {
        if (requireNamespace("irlba", quietly = TRUE)) {
            set.seed(42)
            prval <- irlba::irlba(X.rec, nv = n.eof)
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
            return(X.rec)
        }
    }
}
