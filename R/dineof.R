## DNIEOF
# Based on http://menugget.blogspot.com/2012/10/dineof-data-interpolating-empirical.html#more
# http://journals.ametsoc.org/doi/full/10.1175/1520-0426%282003%29020%3C1839%3AECADFF%3E2.0.CO%3B2



ImputeEOF <- function(data, formula, value.var, max.eof = length(X),
                      min.eof = 1, tol = 1e-4, max.iter = 10000,
                      validation = NULL, verbose = FALSE) {
    # Build matrix if necessary.
    if (is.matrix(data)) {
        X <- data
    } else if (is.data.frame(data)) {
        nas <- sum(is.na(data[[value.var]]))
        if (nas == 0) {
            warning("Data has no missing values")
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
    for (i in 1:max.iter) {
        svd <- irlba::irlba(X.rec, neig = n.eof)
        R <- svd$u%*%diag(svd$d, nrow = n.eof)%*%t(svd$v)
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


# Turns tidy field to matrix + 2 data frames of row and column dimensions
.tidy2matrix <- function(data, formula, value.var) {
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])

    g <- dcast(setDT(data), formula, value.var = value.var)

    dims <- list()
    if (length(col.vars) > 1) {
        cols <- unlist(strsplit(colnames(g), split = "_"))
    } else {
        cols <- colnames(g)
    }

    for (i in seq_along(col.vars)) {
        dims[[i]] <- JumpBy(cols, length(col.vars), start = i + length(row.vars))
        if (class(data[[col.vars[i]]]) != "Date") {
            dims[[i]] <- as(dims[[i]], class(data[[col.vars[i]]]))
        } else {
            dims[[i]] <- as.Date(dims[[i]])
        }
    }
    names(dims) <- col.vars
    return(list(matrix = as.matrix(g[, -seq_along(row.vars), with = F]),
                coldims = dims,
                rowdims = as.list(g[, row.vars, with = F])))
}
