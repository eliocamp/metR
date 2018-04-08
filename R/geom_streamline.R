
stat_streamline <- function(mapping = NULL, data = NULL,
                            geom = "streamline", position = "identity",
                            ...,
                            L = 5,
                            res = 1,
                            skip = 1,
                            skip.x = skip,
                            skip.y = skip,
                            n = NULL,
                            nx = n,
                            ny = n,
                            arrow.angle = 10,
                            arrow.length = 0.5,
                            arrow.ends = "last",
                            arrow.type = "closed",
                            arrow = grid::arrow(arrow.angle, unit(arrow.length, "lines"),
                                                ends = arrow.ends, type = arrow.type),
                            lineend = "butt",
                            na.rm = T,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatStreamline,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            L = L,
            res = res,
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            skip.x = skip.x,
            skip.y = skip.y,
            nx = nx,
            ny = ny,
            ...
        )
    )
}


geom_streamline <-  function(mapping = NULL, data = NULL,
                             stat = "streamline", position = "identity",
                             ...,
                             L = 5,
                             res = 1,
                             skip = 1,
                             skip.x = skip,
                             skip.y = skip,
                             n = NULL,
                             nx = n,
                             ny = n,
                             arrow.angle = 10,
                             arrow.length = 0.5,
                             arrow.ends = "last",
                             arrow.type = "closed",
                             arrow = grid::arrow(arrow.angle, unit(arrow.length, "lines"),
                                                 ends = arrow.ends, type = arrow.type),
                             lineend = "butt",
                             na.rm = T,
                             show.legend = NA,
                             inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomStreamline,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            L = L,
            res = res,
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            skip.x = skip.x,
            skip.y = skip.y,
            nx = nx,
            ny = ny,
            ...
        )
    )
}


StatStreamline <- ggplot2::ggproto("StatStreamline", ggplot2::Stat,
                                   required_aes = c("x", "y", "dx", "dy"),
                                   setup_params = function(data, params) {

                                       M <- with(data, mean(Mag(dx, dy), na.rm = T))  # No me gustaaaa

                                       r <- min(ggplot2::resolution(data$x, zero = FALSE),
                                                ggplot2::resolution(data$y, zero = FALSE))

                                       params$S <- ceiling(params$L*params$res/r)
                                       if (params$S == 1) warning("performing only 1 integration step, please consider increasing the resolution")
                                       params$dt <- params$L/params$S/M

                                       return(params)
                                   },
                                   compute_group = function(data, scales, dt = 0.1, S = 3, skip.x = 1,
                                                            skip.y = 1, nx = 10, ny  = 10,
                                                            L = NULL, res = NULL) {

                                       data <- streamline.f(data, dt = dt, S = S, skip.x = skip.x,
                                                            skip.y = skip.y, nx = nx, ny = ny)

                                       return(data)
                                   }
)


GeomStreamline <- ggplot2::ggproto("GeomStreamline", ggplot2::GeomPath,
                                   default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA,
                                                     group = group)
)


#' @importFrom stats rnorm
#' @importFrom fields interp.surface
streamline <- function(field, dt = 0.1, S = 3, skip.x = 1, skip.y = 1, nx = NULL,
                       ny = NULL) {

    field <- copy(as.data.table(field))
    field <- field[!is.na(dx) & !is.na(dy)]

    dx.field <- .tidy2matrix(field, x ~ y, value.var = "dx")
    dx.field <- list(x = dx.field$rowdims[[1]],
                     y =  dx.field$coldims[[1]],
                     z =  dx.field$matrix)
    dy.field <- .tidy2matrix(field, x ~ y, value.var = "dy")
    dy.field <- list(x = dy.field$rowdims[[1]],
                     y =  dy.field$coldims[[1]],
                     z =  dy.field$matrix)

    force.fun <- function(x, y) {
        dx <- fields::interp.surface(dx.field, cbind(x, y))
        dy <- fields::interp.surface(dy.field, cbind(x, y))
        return(list(dx = dx, dy = dy))
    }

    # Build grid
    if (is.null(nx)) {
        x <- JumpBy(unique(field$x), skip.x + 1)
    } else {
        range.x <- range(field$x)
        x <- seq(range.x[1], range.x[2], length.out = nx)
    }
    if (is.null(ny)) {
        y <- JumpBy(unique(field$y), skip.y + 1)
    } else {
        range.y <- range(field$y)
        y <- seq(range.y[1], range.y[2], length.out = ny)
    }
    rx <- ggplot2::resolution(field$x, zero = FALSE)
    ry <- ggplot2::resolution(field$y, zero = FALSE)

    points <- as.data.table(expand.grid(x = x, y = y))

    set.seed(42)
    points[, x := x + rnorm(.N, rx, rx)]
    points[, y := y + rnorm(.N, ry, ry)]
    points[, c("dx", "dy") := force.fun(x, y)]
    points <- points[dx + dy != 0 & !is.na(dx) & !is.na(dy)]
    points[, group := 1:.N]
    points[, sim := 0]

    points2 <- copy(points)

    # points <- as.list(expand.grid(x = x, y = y))
    # set.seed(42)
    # N <- length(points$x)
    # points$x <- with(points, x + rnorm(N, rx, rx))
    # points$y <- with(points, y + rnorm(N, ry, ry))
    #
    # points[c("dx", "dy")] <- with(points, force.fun(x, y, field))
    # keep <- with(points, dx + dy != 0 & !is.na(dy) & !is.na(dx))
    # points <- lapply(points, function(o) o[keep])
    # N <- length(points$x)
    #
    # points <- lapply(points, function(o) c(o, rep(NA, N*S)))
    #
    # prev <- 1:N

    # Integration
    for (s in 1:S) {
        points2[, c("x", "y", "sim") := .(x + dx*dt, y + dy*dt, s)]
        points2[, c("dx", "dy") := force.fun(x, y)]
        points2 <- points2[!is.na(dx) & !is.na(dy)]
        points <- rbindlist(list(points, points2)) # se puede optimizar prealocando
        points2 <- points2[dx + dy != 0]

        # now <- (N*s + 1):(N*(s+1))
        # points$x[now] <- with(points, x[prev] + dx[prev]*dt)
        # points$y[now] <- with(points, y[prev] + dy[prev]*dt)
        #
        # f <- with(points, force.fun(x[now], y[now], field))
        # points$dx[now] <- ifelse(is.na(f$dx), 0, f$dx)
        # points$dy[now] <- ifelse(is.na(f$dy), 0, f$dy)
        #
        # prev <- now
    }

    points <- as.data.table(points)


    return(setDF(points[, .(x, y, group, sim)]))
}

streamline.f <- memoise::memoise(streamline)

dx <- function(dlon, dx, lat, a = 6731000) {
    if (hasArg(dlon)) {
        return(dlon*pi/180*a*cos(lat*pi/180))
    } else if (hasArg(dx)) {
        return(dx/(a*cos(lat*pi/180))*180/pi)
    }
}

dy <- function(dlat, dy, a = 6731000) {
    if (hasArg(dlat)) {
        return(dlat*a*pi/180)
    } else if (hasArg(dy)) {
        return(dy/a*180/pi)
    }
}
