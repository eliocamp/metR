#' Helpful wrapers for commonly used functions.
#'
#' @name utilities
NULL

#' @rdname utilities
#' @export
#' @aliases Anomaly
Anomaly <- function(x) {
    as.numeric(scale(x, scale = F))
}

#' @rdname utilities
#' @export
Percentile <- function(x) {
    ecdf(x)(x)
}

#' @rdname utilities
#' @export
Mag <- function(x, y) {
    sqrt(x^2 + y^2)
}


#' @rdname utilities
#' @export
`%~%` <- function(x, target) {
    x <- abs(x - target)
    return(x == min(x))
}

#' @rdname utilities
#' @export
`%b%` <- function(x, limits) {
    # Operador "between"
    return(x >= min(limits) & x <= max(limits))
}



#' @rdname utilities
#' @export
JumpBy <- function(x, by, fill = c("skip", NA)) {
    if (is.na(fill[1])) {
        x[-seq(1, length(x), by = by)] <- NA
    } else {
        x <- x[seq(1, length(x), by = by)]
    }
    return(x)
}
