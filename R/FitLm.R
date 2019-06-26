#' Fast estimates of linear regression
#'
#' Computes a linear regression with [stats::.lm.fit] and returns the estimate
#' and, optionally, standard error for each regressor.
#'
#' @param y numeric vector of observations to model
#' @param ... numeric vectors of variables used in the modeling
#' @param se logical indicating whether to compute the standard error
#'
#' @return
#' a list with elements
#' \describe{
#'    \item{term}{the name of the regressor}
#'    \item{estimate}{estimate of the regression}
#'    \item{std.error}{standard error}
#'    \item{df}{degrees of freedom}
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
#' ggplot(regr[term != "(intercept)"], aes(lon, lat)) +
#'     geom_contour(aes(z = estimate, color = ..level..)) +
#'     stat_subset(aes(subset = abs(estimate) > 2*std.error), size = 0.05)
#'
#' # Using stats::lm() is much slower and with no names.
#' \dontrun{
#' system.time({
#'   regr <- geopotential[, coef(lm(gh ~ date)), by = .(lon, lat)]
#' })
#' }
#'
#' @export
#' @importFrom stats .lm.fit complete.cases
FitLm <- function(y, ..., se = FALSE) {
    X <- cbind(`(Intercept)` = 1, ...)
    term <- dimnames(X)[[2]]
    remove <- which(!complete.cases(X) | is.na(y))
    N <- length(y) - length(remove)

    # If empty, reurn NA with a warning.
    if (N < 2) {
        estimate <- rep(NA_real_, length(term))
        if (se == TRUE) {
            se <- estimate
            df <- N - ncol(X)
            return(list(term = term,
                        estimate = estimate,
                        std.error = se,
                        df = rep(df, length(term))))
        } else {
            return(list(term = term,
                        estimate = estimate))
        }
    } else {
        if (length(remove) > 0) {
            X <- X[-remove, ]
            y <- y[-remove]
        }

        a <- .lm.fit(X, y)
        estimate <- a$coefficients
    }

    if (se == TRUE) {
        df <- N - ncol(X)
        if (all(a$residuals == 0)) {
            se <- NA_real_
        } else {
            sigma <- sum(a$residuals^2)/(nrow(X) - ncol(X))
            se <- sqrt(diag(chol2inv(chol(t(X)%*%X)))*sigma)
        }
        return(list(term = term,
                    estimate = estimate,
                    std.error = se,
                    df = rep(df, length(term))))
    } else {
        return(list(term = term,
                    estimate = estimate))
    }
}
