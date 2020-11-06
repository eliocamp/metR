#' Compute trajectories
#'
#' Computes trajectories of particles in a time-varying velocity field.
#'
#' @param formula a formula indicating dependent and independent variables
#' in the form of dx + dy ~ x + y + t.
#' @param x0,y0 starting coordinates of the particles.
#' @param cyclical logical vector of boundary condition for x and y.
#' @param data optional data.frame containing the variables.
#' @param res resolution parameter (higher numbers increases the resolution)
#'
#'
#' @export
Trajectory <- function(formula, x0, y0,
                       cyclical = FALSE,
                       data = NULL, res = 2) {

    checks <- makeAssertCollection()

    assertClass(formula, "formula", add = checks)
    assertSameLength(list(x0 = length(x0), y0 = length(y0)), add = checks)
    assertLogical(cyclical, add = checks)
    assertDataFrame(data, null.ok = TRUE, add = checks)
    assertNumeric(res, len = 1, add = checks)

    reportAssertions(checks)

    dep.names <- formula.tools::lhs.vars(formula)
    if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")

    if (length(dep.names) != 2) {
        stop("LHS of formula must be of the form dx + dy")
    }

    ind.names <- formula.tools::rhs.vars(formula)
    if (length(ind.names) != 3) {
        stop("RHS of formula must be of the form x + y + t")
    }

    formula <- Formula::as.Formula(formula)
    field <- data.table::as.data.table(eval(quote(model.frame(formula, data = data,
                                                  na.action = NULL))))

    data.table::setorderv(field, ind.names)
    data.table::setnames(field, c(ind.names, dep.names), c("x", "y", "t", "dx", "dy"))

    if (.is.somedate(field$t)) {
        field[, t1 := to_seconds(t[1]), by = t]
    } else {
        field[, t1 := t]
    }

    field <- field[!is.na(dx) & !is.na(dy)]

    range.x <- range(field$x)
    range.y <- range(field$y)

    times <- unique(field$t1)
    dt <- diff(times)[1]/res
    ts <- seq(min(times), max(times), by = dt)

    points_out <- list(data.table::data.table(x = x0, y = y0, id = seq_along(x0),
                                  piece = 1,
                                  t1 = ts[1]))
    points_out[[1]][, c("dx", "dy") := force.fun3d(x, y, t1, times, field)]
    points <- data.table::copy(points_out[[1]])

    if (length(cyclical) == 1) {
        cyclical <- rep(cyclical, 2)
    }
    circ.x <-  cyclical[1]
    circ.y <-  cyclical[2]

    for (ti in seq_along(ts)[-1]) {
        points[, c("x", "y", "t1") := .(x + dx*dt, y + dy*dt, ts[ti])]
        points[, c("x", "piece") := .fold(x, piece, range.x, circ.x)]
        points[, c("y", "piece") := .fold(y, piece, range.y, circ.y)]
        points[, c("dx", "dy") := force.fun3d(x, y, ts, times, field)]
        points <- points[!is.na(dx) & !is.na(dy)]
        points_out[[ti]] <- points
        points <- points[dx + dy != 0]
        if(nrow(points) == 0) break
    }

    points_out <- do.call(rbind, points_out)

    if (.is.somedate(field$t)) {
        points_out[, t1 := start <- lubridate::ymd_hms("1970-01-01 00:00:00") +
                       lubridate::seconds(t1[1]),
                   by = t1]
    }

    data.table::setnames(points_out, c("x", "y", "t1", "dx", "dy"), c(ind.names, dep.names))

    return(points_out[])
}

force.fun3d <- function(x, y, ts, times, field) {
    # si t justo está en times, es el mismo, así que no hace falta
    # interpolar nada
    ts <- ts[1]
    if (isTRUE(ts %in% times)) {
        force.fun(x, y, field[t1 == ts])
    } else {
        # Agarro los tiempos anteriores y siguientes, calculo las fuerzas
        # en x e y y luego interpolo a ts
        t0 <- times[times <= ts]
        t0 <- t0[length(t0)] # tiempo anterior
        tf <- times[times >= ts][1]  # tiempo siguiente

        t0_force <- data.table::setDT(force.fun(x, y, field[t1 == t0]))
        data.table::set(t0_force, NULL, "x", x)
        data.table::set(t0_force, NULL, "y", y)
        data.table::set(t0_force, NULL, "t", t0)

        tf_force <- data.table::setDT(force.fun(x, y, field[t1 == tf]))
        data.table::set(tf_force, NULL, "x", x)
        data.table::set(tf_force, NULL, "y", y)
        data.table::set(tf_force, NULL, "t", tf)

        force <- rbind(data.table::setDT(t0_force), data.table::setDT(tf_force))
        force <- force[, .(dx = mean(dx),
                           dy = mean(dy)),
                       by = .(x, y)]
        return(force[, .(dx, dy)])
    }
}

force.fun <- function(x, y, field) {
    matrix <- .tidy2matrix(field, x ~ y, value.var = "dx", fill = 0)
    dx.field <- list(x = matrix$rowdims$x,
                     y = matrix$coldims$y,
                     z = matrix$matrix)

    matrix <- .tidy2matrix(field, x ~ y, value.var = "dy", fill = 0)
    dy.field <- list(x = matrix$rowdims$x,
                     y = matrix$coldims$y,
                     z = matrix$matrix)

    dx <- fields::interp.surface(dx.field, cbind(x, y))
    dy <- fields::interp.surface(dy.field, cbind(x, y))
    return(list(dx = dx, dy = dy))
}

to_seconds <- function(date) {
    start <- lubridate::ymd_hms("1970-01-01 00:00:00")
    lubridate::time_length(lubridate::interval(start, date), "second")
}
