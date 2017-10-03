#' Reverse log transform
#'
#' Reverse log transformation. Useful when plotting and one axis is in pressure
#' levels.
#'
#' @param base Base of the logarithm
#'
#' @examples
#' # Adiabatic temperature profile
#' gamma <- 0.286
#' t <- data.frame(p = c(1000, 950, 850, 700, 500, 300, 200, 100))
#' t$t <- 300*(t$p/1000)^gamma
#'
#' library(ggplot2)
#' ggplot(t, aes(p, t)) +
#'    geom_line() +
#'    coord_flip() +
#'    scale_x_continuous(trans = "reverselog")
#'
#' @family ggplot2 helpers
#' @export
#' @import scales
reverselog_trans <- function(base = 10) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)

    scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
              scales::log_breaks(base = base),
              domain = c(1e-100, Inf))
}
