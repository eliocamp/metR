#' Functions to place contour labels
#'
#' These functions compute the position of contour labels
#'
#' @param frac A numeric vector with values between 0 and 1 representing
#' where in the contour to put labels (i.e. `frac = 0.5` puts labels
#' at the midpoint).
#' @param n Number of labels to put.
#' @param seed Seed to use for randomly choosing where to put labels.
#' @param ref_angle Angle (in degrees counter-clockwise from East) to try
#' to approximate labels.
#'
#'
#'
#'
#' @export
#' @rdname contour_placement
label_placement_fraction <- function(frac = 0.5) {
    checkmate::assert_numeric(frac, lower = 0, upper = 1)
    range(frac)
    force(frac)
    function(x, y) {
        d <-  cumsum(sqrt(c(0, diff(x))^2 + c(0, diff(y))^2))
        places <- vapply(frac, function(x) which.min(abs(d - max(d*x))), numeric(1))
        return(places)
    }
}

#' @export
#' @rdname contour_placement
label_placement_n <- function(n = 2) {
    force(n)
    force(start)
    start <- 1/(n*2)
    function(x, y) {
        N <- length(x)
        start <- start*N + 1
        by <- floor(N/n)
        seq(start, N, by = by)
    }
}


#' @export
#' @rdname contour_placement
label_placement_random <- function(seed = 42, n = 1) {
    force(n)
    force(seed)
    function(x, y) {
        i <- seq_along(x)
        selected <- with_seed(seed, sample(i, n))
        i %in% selected
    }
}

with_seed <- function(seed, expr) {
    old_seed <- get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)

    on.exit({
        if (is.null(old_seed)) {
            rm(".Random.seed", envir = globalenv())
        } else {
            assign(".Random.seed", old_seed, globalenv())
        }
    })
    set.seed(seed)
    expr
}

#' @export
#' @rdname contour_placement
label_placement_all <- function() {
    function(x, y) {
        rep(TRUE, length(x))
    }
}

#' @export
#' @rdname contour_placement
label_placement_flattest <- function(n = 1, ref_angle = 0) {
    force(ref_angle)
    function(x, y) {
        if (length(x) < 6) {
            return(FALSE)
        }
        i <- seq_along(x)

        # from https://www.math24.net/curvature-radius/
        dx <- .derv(x, i)
        dy <- .derv(y, i)
        ddx <- .derv(dx, i)
        ddy <- .derv(dy, i)

        curvature <- abs(dx*ddy - dy*ddx)/(dx^2 + dy^2)^(3/2)
        curvature <- atan(curvature)

        angle <- abs(atan(dy/dx) - ref_angle*pi/180)

        optim <- curvature + angle
        min <- min(optim, na.rm = TRUE)

        possibile <- which(optim == min)

        distance <- cumsum(sqrt(c(0, diff(x))^2 + c(0, diff(y))^2))
        midpoint <- which.min(abs(distance - max(distance)/2))

        best <- possibile[which.min(abs(distance[possibile] - midpoint))]

        return(best)
    }
}
