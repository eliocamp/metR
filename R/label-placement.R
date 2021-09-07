#' Functions to place contour labels
#'
#' These functions compute the position of contour labels.
#'
#' @param frac A numeric vector with values between 0 and 1 representing
#' where in the contour to put labels (i.e. `frac = 0.5` puts labels
#' at the midpoint).
#' @param n Number of labels to put.
#' @param seed Seed to use for randomly choosing where to put labels.
#' @param ref_angle Angle (in degrees counter-clockwise from East) to try
#' to approximate labels.
#' @param rot_adjuster A function that standardizes the rotation angles of the labels.
#' See e.g. [isoband::angle_halfcircle_bottom].
#'
#' @export
#' @rdname contour_placement
label_placer_fraction <- function(frac = 0.5, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_fraction_xy(frac = frac), rot_adjuster = rot_adjuster)
}

#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_fraction <- function(frac = 0.5, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_fraction")
    label_placer_fraction(frac, rot_adjuster)
}

# the _xy functions take x and y vectors and return the index of
# the points to label.
label_placer_fraction_xy <- function(frac = 0.5) {
    checkmate::assert_numeric(frac, lower = 0, upper = 1)
    range(frac)
    force(frac)
    function(x, y) {
        d <-  cumsum(sqrt(c(0, diff(x))^2 + c(0, diff(y))^2))
        places <- vapply(frac, function(x) which.min(abs(d - max(d*x))), numeric(1))
        return(places)
    }
}

label_placer_n_xy <- function(n = 2) {
    force(n)
    force(start)
    start <- 1/(n*2)
    function(x, y) {
        N <- length(x)
        start <- start*N + 1
        by <- floor(N/n)
        as.integer(seq(start, N, by = by))
    }
}

#' @export
#' @rdname contour_placement
label_placer_n <- function(n = 2, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_n_xy(n = n), rot_adjuster = rot_adjuster)
}


#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_n <- function(n = 2, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_n")
    label_placer_n(n = n, rot_adjuster = rot_adjuster)
}

label_placer_random_xy <- function(seed = 42, n = 1) {
    force(n)
    force(seed)
    function(x, y) {
        i <- seq_along(x)
        selected <- with_seed(seed, sample(i, n))
        which(i %in% selected)
    }
}

#' @export
#' @rdname contour_placement
label_placer_random <- function(seed = 42, n = 1, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_random_xy(seed = seed, n = n), rot_adjuster = rot_adjuster)
}

#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_random <- function(seed = 42, n = 1, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_random")
    label_placer_random(seed = seed, n = n, rot_adjuster = rot_adjuster)
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


label_placer_all_xy <- function() {
    function(x, y) {
        rep(TRUE, length(x))
    }
}

#' @export
#' @rdname contour_placement
label_placer_all <- function(rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_all_xy(), rot_adjuster = rot_adjuster)
}

#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_all <- function(rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_all")
    label_placer_all(rot_adjuster = rot_adjuster)
}

label_placer_flattest_xy <- function(ref_angle = 0) {
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

#' @export
#' @rdname contour_placement
label_placer_flattest <- function(ref_angle = 0, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_flattest_xy(ref_angle = ref_angle), rot_adjuster = rot_adjuster)
}

#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_flattest <-  function(ref_angle = 0, rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_flattest")
    label_placer_flattest(ref_angle = ref_angle, rot_adjuster = rot_adjuster)
}

label_placer_minmax_xy <- function(direction = c("vertical", "horizontal")) {
    direction <- direction[1]
    assertChoice(direction, c("vertical", "horizontal"))

    function(x, y) {
        if (direction == "vertical") {
            return(c(which.max(y), which.min(y)))
        } else if (direction == "horizontal") {
            return(c(which.max(x), which.min(x)))
        }
    }

}

#' @param direction Direction in which to compute the maximum and minimum.
#' @export
#' @rdname contour_placement
label_placer_minmax <- function(direction = c("vertical", "horizontal"),
                                rot_adjuster = isoband::angle_halfcircle_bottom()) {
    as_placer(label_placer_minmax_xy(direction = direction),
                         rot_adjuster = rot_adjuster)
}

#' @export
#' @keywords internal
#' @rdname contour_placement
label_placement_minmax <-  function(direction = c("vertical", "horizontal"),
                                    rot_adjuster = isoband::angle_halfcircle_bottom()) {
    .Deprecated("label_placer_minmax")
    label_placer_minmax(direction = direction, rot_adjuster = rot_adjuster)
}


as_placer <- function(fun, rot_adjuster) {
    force(fun)

    if (is.numeric(rot_adjuster)) {
        angle <- rot_adjuster
        rot_adjuster <- function(x) angle*pi/180
    }

    placer_fun <- function(line_data, ...) {
        ids <- unique(line_data$id)
        out <- data.frame(x = numeric(0), y = numeric(0), theta = numeric(0))

        for (id in seq_along(ids)) {
            i <- which(line_data$id == ids[id])
            x <- line_data$x[i]
            y <- line_data$y[i]
            selected <- fun(x, y)
            theta <- compute_angle(x, y, selected)

            out <- rbind(out, list(x = x[selected], y = y[selected], theta = theta))
        }

            out$theta <- rot_adjuster(out$theta)


        out
    }

    function(lines, labels_data) {
        isoband::label_placer_simple(lines, labels_data, placer_fun)
    }
}


compute_angle <- function(x, y, idx) {
    idx_min <- 1
    idx_max <- length(x)
    n <- 2
    vapply(idx, function(idx) {
        if (x[idx_min] == x[idx_max] && y[idx_min] == y[idx_max]) {
            idx_range <- (idx_max - idx_min)
            i <- ((idx - n):(idx + n)-idx_min) %% idx_range + idx_min
        } else {
            i <- (max(idx - n, idx_min):min(idx + n, idx_max))
        }

        x <- x[i]
        y <- y[i]
        xave <- mean(x)
        yave <- mean(y)
        m <- cbind(x - xave, y - yave)
        v <- svd(m)$v
        atan2(v[2], v[1])
    }, numeric(1))

}
