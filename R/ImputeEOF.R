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
#' @param validation number of points to use in crossvalidation (defaults to
#' 30 or 10\% of the non NA points)
#' @param verbose logical indicating whether to print progress
#'
#' @return
#' A data table with imputed values.
#'
#' @details
#' If `data` is a matrix, the `formula` argument is ignored and the function
#' returns a matrix.
#'
#' @references
#' Beckers, J.-M., Barth, A., and Alvera-Azcárate, A.: DINEOF reconstruction of clouded images including error maps – application to the Sea-Surface Temperature around Corsican Island, Ocean Sci., 2, 183-199, https://doi.org/10.5194/os-2-183-2006, 2006.
#'
#' @examples
#' library(data.table)
#' aao <- copy(aao)
#' aao[, gh.t := Anomaly(gh), by = .(lat, lon)]
#' aao <- aao[date == date[1]]
#'
#' # Add gaps to field
#' aao[, gh.gap := gh.t]
#' aao[sample(1:.N, .N*0.3), gh.gap := NA]
#'
#' aao.full <- as.data.table(ImputeEOF(aao, lon ~ lat,  value.var = "gh.gap",
#'                                     verbose = TRUE, max.iter = 2000))
#'
#' aao <- aao[aao.full[, .(lon, lat, gh.impute = gh.gap)], on = c("lon", "lat")]
#'
#' library(ggplot2)
#' ggplot(aao, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.t), color = "black") +
#'     geom_contour(aes(z = gh.impute))
#'
#' ggplot(aao[is.na(gh.gap)], aes(gh.t, gh.impute)) +
#'     geom_point()
#'
#' @import data.table
#' @export
ImputeEOF <- function(data, formula, value.var, max.eof = length(X),
                      min.eof = 1, tol = 1e-4, max.iter = 10000,
                      validation = NULL, verbose = FALSE) {
    # Build matrix if necessary.
    if (is.matrix(data)) {
        X <- data
    } else if (is.data.frame(data)) {
        nas <- sum(is.na(data[[value.var]]))
        if (nas == 0) {
            warning("data has no missing values")
            return(data)
        }
        g <- .tidy2matrix(data, formula, value.var)
        X <- g$matrix
    } else {
        stop("data argument must be matrix or data frame")
    }

    gaps <- which(is.na(X))
    if (length(gaps) == 0) return(X)

    if (is.null(validation)) {
        validation <- max(30, 0.1*length(X[-gaps]))
    }
    set.seed(42)    # let's get reproducible in here.
    validation <- sample((1:length(X))[!1:length(X) %in% gaps],
                         validation)

    eofs <- 0:max.eof
    X.rec <- X
    # First try, imput with mean or something. Rmse is infinite.
    fill <- 0
    X.rec[c(gaps, validation)] <- fill
    rmse <- Inf

    for (i in 2:length(eofs)) {
        # After first guess, impute gaps and validation.
        X.rec.temp <- try(.ImputeEOF1(X.rec, c(gaps, validation), eofs[i],
                                      tol = tol, max.iter = max.iter,
                                      verbose = verbose))
        # If it doesn't converge, then rmse is infinite
        if (is.error(X.rec.temp)) {
            rmse <- c(rmse, Inf)
        } else {
            X.rec <- X.rec.temp
            rmse <- c(rmse, sqrt(mean((X[validation] - X.rec[validation])^2)))
        }
        if (verbose == TRUE) {
            cat("\r", "With", eofs[i], "eof - convergence:", !is.error(X.rec.temp))
        }


        # Break the loop if we are over the minimum eof asked and, either
        # this rmse is greater than the previous one or current rmse is inf.
        if (eofs[i] > min.eof & (rmse[i] > rmse[i - 1] | rmse[i] == Inf)) {
            break
        }
    }
    # Select best eof and make proper imputation.
    eof <- eofs[which.min(rmse)]
    X[gaps] <- fill
    X.rec <- .ImputeEOF1(X, gaps, eof, tol = tol, max.iter = max.iter)

    if (is.data.frame(data)) {
        dimnames(X.rec) <- list(unlist(g$rowdims), unlist(g$coldims))
        names(dimnames(X.rec)) <- list(names(g$rowdims), names(g$coldims))
        X.rec <- data.table::melt(X.rec, value.name = value.var)
    }

    attr(X.rec, "eof") <- eof
    attr(X.rec, "rmse") <- min(rmse)
    return(X.rec)
}


.ImputeEOF1 <- function(X, X.na, n.eof, tol = 1e-4, max.iter = 10000, verbose = TRUE) {
    X.rec <- X
    v <- NULL
    for (i in 1:max.iter) {
        if (requireNamespace("irlba", quietly = TRUE)) {
            prval <- irlba::irlba(X.rec, nv = n.eof, v = v)
        } else {
            prval <- prval(X.rec, nu = n.eof, nv = n.eof)
            prval$d <- prval$d[1:n.eof]
        }
        v <- prval$v
        R <- prval$u%*%diag(prval$d, nrow = n.eof)%*%t(v)
        rmse <- sqrt(mean((R[X.na] - X.rec[X.na])^2))

        X.rec[X.na] <- R[X.na]

        if (rmse < tol) {
            X[X.na] <- X.rec[X.na]
            return(X)
        }
    }
    if (verbose == TRUE) {
        stop(paste0("Algorithm failed to converge after ", max.iter, " iterations"))
    } else {
        stop()
    }

}
