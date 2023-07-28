#' Fast estimates of linear regression
#'
#' Computes a linear regression with [stats::.lm.fit] and returns the estimate
#' and, optionally, standard error for each regressor.
#'
#' @param y numeric vector of observations to model
#' @param ... numeric vectors of variables used in the modelling
#' @param intercept logical indicating whether to automatically add the intercept
#' @param se logical indicating whether to compute the standard error
#' @param r2 logical indicating whether to compute r squared
#' @param weights numerical vector of weights (which doesn't need to be normalised)
#' @param time time vector to use for detrending. Only necessary in the case of
#' irregularly sampled timeseries
#'
#' @return
#' FitLm returns a list with elements
#' \describe{
#'    \item{term}{the name of the regressor}
#'    \item{estimate}{estimate of the regression}
#'    \item{std.error}{standard error}
#'    \item{df}{degrees of freedom}
#'    \item{r.squared}{Percent of variance explained by the model (repeated in each term)}
#'    \item{adj.r.squared}{ r.squared` adjusted based on the degrees of freedom)}
#' }
#'
#' ResidLm and Detrend returns a vector of the same length
#'
#' If there's no complete cases in the regression, `NA`s are returned with no
#' warning.
#'
#' @examples
#' # Linear trend with "signficant" areas shaded with points
#' library(data.table)
#' library(ggplot2)
#' system.time({
#'   regr <- geopotential[, FitLm(gh, date, se = TRUE), by = .(lon, lat)]
#' })
#'
#' ggplot(regr[term != "(Intercept)"], aes(lon, lat)) +
#'     geom_contour(aes(z = estimate, color = after_stat(level))) +
#'     stat_subset(aes(subset = abs(estimate) > 2*std.error), size = 0.05)
#'
#' # Using stats::lm() is much slower and with no names.
#' \dontrun{
#' system.time({
#'   regr <- geopotential[, coef(lm(gh ~ date))[2], by = .(lon, lat)]
#' })
#' }
#'
#' @export
FitLm <- function(y, ..., intercept = TRUE, weights = NULL, se = FALSE, r2 = se) {
    .FitLm(y = y, ..., intercept = intercept, weights = weights, se = se, r2 = r2, resid = FALSE)
}

#' @export
#' @rdname FitLm
ResidLm <- function(y, ..., intercept = TRUE, weights = NULL) {
    .FitLm(y = y, ..., intercept = intercept, weights = weights, resid = TRUE)
}

#' @export
#' @rdname FitLm
Detrend <- function(y, time = seq_along(y)) {
    m <- mean(y, na.rm = TRUE)
    ResidLm(y, time) + m
}

.FitLm <- function(y, ..., intercept = TRUE, weights = NULL, se = FALSE, r2 = se, resid = FALSE) {
    if (isTRUE(intercept)) {
        X <- cbind(`(Intercept)` = intercept, ...)
    } else {
        X <- cbind(...)
    }

    has_weights <- !is.null(weights)

    term <- dimnames(X)[[2]]
    missing <- term == ""
    term[missing] <- paste0("V", seq_len(sum(missing)))

    remove <- which(!stats::complete.cases(X) | is.na(y))
    N <- length(y) - length(remove)
    residuals <- rep(NA_real_, length(y))
    # If empty, return NA
    if (N < 2) {
        if (resid) {
            return(residuals)
        }
        estimate <- rep(NA_real_, length(term))
        out <- list(term = term,
                    estimate = estimate)
        if (se == TRUE) {
            df <- N - ncol(X)
            out$std.error <-  estimate
            out$df <- rep(df, length(term))
        }

        if (r2 == TRUE) {
            out$r.squared <- estimate
            out$adj.r.squared <- estimate
        }

        return(out)
    } else {
        if (length(remove) > 0) {
            X <- X[-remove, ]
            y <- y[-remove]
            if (has_weights) {
                weights <- weights[-remove]
            }

        }

        if (has_weights) {
            weights <- weights/sum(weights)
            fit <- stats::lm.wfit(X, y, w = weights)
        } else {
            fit <- stats::.lm.fit(X, y)
        }

        estimate <- unname(fit$coefficients)
    }

    if (resid) {
        if (length(remove) > 0) {
            residuals[-remove] <- fit$residuals
        }
        residuals <- fit$residuals

        return(residuals)
    }

    out <- list(term = term,
                estimate = estimate)

    if (se == TRUE | r2 == TRUE) {
        df <- N - ncol(X)

        if (has_weights) {
            res_sum <- sum(weights*fit$residuals^2)
            ss <- sum(weights*(y - stats::weighted.mean(y, w = weights))^2)
        } else {
            res_sum <- sum(fit$residuals^2)
            ss <- sum((y - mean(y))^2)
        }


        if (se == TRUE) {
            if (all(fit$residuals == 0)) {
                out$std.error <-  rep(NA_real_, length(term))
                out$df <-  rep(NA_integer_, length(term))
            } else {
                p <- seq_len(fit$rank)
                if (is.qr(fit$qr)) fit$qr <- fit$qr$qr
                R <- chol2inv(fit$qr[p, p, drop = FALSE])
                std.error <- sqrt(diag(R) * res_sum/df)
                out$std.error <- std.error
                out$df <- rep(df, length(term))
            }
        }


        if (r2 == TRUE) {
            r_squared <- 1 - res_sum/ss
            adj_r_squared <- 1 - res_sum/ss*(N-1)/df
            out$r.squared <- rep(r_squared, length(term))
            out$adj.r.squared <- rep(adj_r_squared, length(term))
        }
    }

    return(out)
}
