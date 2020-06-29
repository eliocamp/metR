#' Fast estimates of linear regression
#'
#' Computes a linear regression with [stats::.lm.fit] and returns the estimate
#' and, optionally, standard error for each regressor.
#'
#' @param y numeric vector of observations to model
#' @param ... numeric vectors of variables used in the modelling
#' @param se logical indicating whether to compute the standard error
#' @param r2 logical indicating whether to compute r squared
#' @param weights numerical vector of weights (which doesn't need to be normalised)
#'
#' @return
#' a list with elements
#' \describe{
#'    \item{term}{the name of the regressor}
#'    \item{estimate}{estimate of the regression}
#'    \item{std.error}{standard error}
#'    \item{df}{degrees of freedom}
#'    \item{r.squared}{Percent of variance explained by the model (repeated in each term)}
#'    \item{adj.r.squared}{ r.squared` adjusted based on the degrees of freedom)}
#' }
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
#'     geom_contour(aes(z = estimate, color = ..level..)) +
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
#' @importFrom stats .lm.fit complete.cases
FitLm <- function(y, ..., weights = rep(1, length(y)), se = FALSE, r2 = se) {
    X <- cbind(`(Intercept)` = 1, ...)
    has_weights <- data.table::uniqueN(weights) > 1

    term <- dimnames(X)[[2]]
    missing <- term == ""
    term[missing] <- paste0("V", seq_len(sum(missing)))

    remove <- which(!complete.cases(X) | is.na(y))
    N <- length(y) - length(remove)

    # If empty, reurn NA with a warning.
    if (N < 2) {
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
            weights <- weights[-remove]
        }

        if (has_weights) {
            weights <- weights/sum(weights)
            fit <- stats::lm.wfit(X, y, w = weights)
        } else {
            fit <- .lm.fit(X, y)
        }

        estimate <- unname(fit$coefficients)
    }

    out <- list(term = term,
                estimate = estimate)

    if (se == TRUE | r2 == TRUE) {
        df <- N - ncol(X)
        res_sum <- sum(weights*fit$residuals^2)
        if (has_weights) {
            ss <- sum(weights*(y - stats::weighted.mean(y, w = weights))^2)
        } else {
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
