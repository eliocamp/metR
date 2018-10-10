
Trajectory <- function(formula, x0, y0, data = NULL, res = 0.5) {
    dep.names <- formula.tools::lhs.vars(formula)
    if (length(dep.names) == 0) stop("LHS of formula must have at least one variable")

    ind.names <- formula.tools::rhs.vars(formula)
    if (length(ind.names) > 3) {
        stop("RHS of formula must be of the form x + y + t")
    }

    formula <- Formula::as.Formula(formula)
    field <- as.data.table(eval(quote(model.frame(formula, data = data,
                                                  na.action = NULL))))

    setorderv(field, ind.names)
    setnames(field, c(ind.names, dep.names), c("x", "y", "t", "dx", "dy"))

    if (.is.somedate(field$t)) {
        field[, t1 := to_seconds(t[1]), by = t]
    } else {
        field[, t1 := t]
    }

    field <- field[!is.na(dx) & !is.na(dy)]

    times <- unique(field$t1)
    dt <- diff(times)[1]*res
    ts <- seq(min(times), max(times), by = dt)

    points_out <- data.table(x = x0, y = y0, id = seq_along(x0),
                             t1 = ts[1])
    points_out[, c("dx", "dy") := force.fun3d(x, y, t1, times, field)]
    points <- copy(points_out)

    for (ti in seq_along(ts)[-1]) {
        points[, t1 := ts[ti]]
        points[, c("x", "y") := .(x + dx*dt, y + dy*dt)]
        points[, c("dx", "dy") := force.fun3d(x, y, ts, times, field)]
        points <- points[!is.na(dx) & !is.na(dy)]
        points_out <- rbindlist(list(points_out, points))
        points <- points[dx + dy != 0]
        if(nrow(points) == 0) break
    }


    return(points_out)
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

        t0_force <- setDT(force.fun(x, y, field[t1 == t0]))
        set(t0_force, NULL, "x", x)
        set(t0_force, NULL, "y", y)
        set(t0_force, NULL, "t", t0)

        tf_force <- setDT(force.fun(x, y, field[t1 == tf]))
        set(tf_force, NULL, "x", x)
        set(tf_force, NULL, "y", y)
        set(tf_force, NULL, "t", tf)

        force <- rbind(setDT(t0_force), setDT(tf_force))
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
    lubridate::time_length(lubridate::interval(lubridate::ymd("1970-01-01"), date), "second")
}
