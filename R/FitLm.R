#' Fast estimates of linear regression
#'
#' Computes a linear regression with [stats::.lm.fit] and returns the estimate
#' and, optionally, standard error for each regressor.
#'
#' @param y numeric vector of observations to model
#' @param ... numeric vectors of variables used in the modeling
#' @param se logical indicating wheter to compute the stantdard error
#'
#' @return
#' a list with elements
#' \describe{
#'    \item{regressor}{the name of the regressor}
#'    \item{estimate}{estiamte of the regression}
#'    \item{se}{standard error}
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
#' ggplot(regr[regressor != "mean"], aes(lon, lat)) +
#'     geom_contour(aes(z = estimate, color = ..level..)) +
#'     stat_subset(aes(subset = abs(estimate) > 2*se), size = 0.05)
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
    X <- cbind(mean = 1, ...)
    regressor <- dimnames(X)[[2]]
    real <- complete.cases(X) & !is.na(y)

    # If empty, reurn NA with a warning.
    if (sum(real) < 2) {
        estimate <- rep(NA_real_, length(regressor))
        if (se == TRUE) {
            se <- estimate
            return(list(regressor = regressor,
                        estimate = estimate,
                        se = se))
        } else {
            return(list(regressor = regressor,
                        estimate = estimate))
        }
    } else {
        X <- X[real, ]
        y <- y[real]
        a <- .lm.fit(X, y)
        estimate <- a$coefficients
    }

    if (se == TRUE) {
        if (all(a$residuals == 0)) {
            se <- NA_real_
        } else {
            sigma <- sum(a$residuals^2)/(nrow(X) - ncol(X))
            se <- sqrt(diag(chol2inv(chol(t(X)%*%X)))*sigma)
        }
        return(list(regressor = regressor,
                    estimate = estimate,
                    se = se))
    } else {
        return(list(regressor = regressor,
                    estimate = estimate))
    }s
}
