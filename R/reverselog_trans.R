#' Reverse log transform
#'
#' Reverse log transformation. Usefull when plotting and one axis is in pressure
#' levels.
#'
#' @param base Base of the logarithm
#'
#' @examples
#' # Adiabatic temperature profile
#' gamma <- 0.286
#' t <- data.frame(p = c(1000, 950, 850, 700, 500, 300, 200, 100))
#' t$t <- 300*(t$p/1000)^gamma
#' ggplot(t, aes(p, t)) +
#'    geom_line() +
#'    coord_flip() +
#'    scale_x_continuous(trans = "reverselog")
#'
#' @export
reverselog_trans <- function(base = 10) {
    library("scales")
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)

    trans_new(paste0("reverselog-", format(base)), trans, inv,
              log_breaks(base = base),
              domain = c(1e-100, Inf))
}


