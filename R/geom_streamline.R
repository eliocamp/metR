#' Streamlines
#'
#' Streamlines are paths that are always tangential to a vector field. In the
#' case of a steady field, it's identical to the path of a massless particle that
#' moves with the "flow".
#'
#' @inheritParams geom_vector
#' @inheritParams ggplot2::stat_identity
#' @param L, tipical length of a streamline in x and y units
#' @param res, resolution parameter (higher numbers increases the resolution)
#' @param S optional numeric number of timesteps for integration
#' @param dt optional numeric size "timestep" for integration
#' @param n,nx,ny optional numeric indicating the number of points to draw in the
#' x and y direction (replaces `skip` if not `NULL`)
#' @param jitter,jitter.x,jitter.y ammount of jitter of the starting poits
#'
#' @details
#' Streamlines are computed by simple integration with a foward Euler method.
#' By default, `stat_streamline()` computes `dt` and `S` from `L`, `res`,
#' the resolution of the grid and the mean magnitude of the field. `S` is
#' then defined as the number of steps necesary to make a streamline of length
#' `L` under an uniform mean field and `dt` is chosen so that each step is no
#' larger than the resolution of the data (divided by the `res` parameter). Be
#' aware that this rule of thumb might fail in field with very skewed distribution
#' of magnitudes.
#'
#' Alternatively, `L` and/or `res` are ignored if `S` and/or `dt` are specified
#' explicitly. This not only makes it possible to fine-tune the result but also
#' divorces the integration parameters from the properties of the data and makes
#' it possible to compare streamlines between different fields.
#'
#' The starting grid is a semi regular grid defined, either by the resolution of the
#' field and the `skip.x` and `skip.y` parameters o the `nx` and `ny` parameters,
#' jittered by an amount proportional to the resolution of the data and the
#' `jitter.x` and `jitter.y` parameters.
#'
#' It might be important that the units of the vector field are compatible to the units
#' of the x and y dimensions. For example, passing `dx` and `dy` in m/s on a
#' longitude-latitude grid will might misleading results (see [spherical]).
#'
#' Missing values are not permited and the field must be defined on a
#' regular grid, for now.
#'
#' @section Aesthetics:
#' `stat_streamline` understands the following aesthetics (required aesthetics are in bold)
#'
#' \itemize{
#' \item **x**
#' \item **y**
#' \item **dx**
#' \item **dy**
#' \item `alpha`
#' \item `colour`
#' \item `linetype`
#' \item `size`
#' }
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' data(geopotential)
#'
#' geopotential <- copy(geopotential)[date == date[1]]
#' geopotential[, gh.z := Anomaly(gh), by = .(lat)]
#' geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]
#'
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.z)) +
#'     geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 50)
#'
#' # If u and v are not converted into degrees/second, the resulting
#' # streamlines have problems, specially near the pole.
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.z)) +
#'     geom_streamline(aes(dx = u, dy = v), L = 50)
#'
#' \dontrun{
#' # Using topographic information to simulate "rivers" from slope
#' topo <- GetTopography(295, -55+360, -30, -42, res = 1/20)  # needs internet!
#' topo[, c("dx", "dy") := Derivate(h ~ lon + lat)]
#' topo[h <= 0, c("dx", "dy") := 0]
#'
#' # See how in this example the integration step is too coarse in the
#' # western montanous region where the slope is much higher than in the
#' # flatlands of La Pampa at in the east.
#' ggplot(topo, aes(lon, lat)) +
#'     geom_relief(aes(z = h), interpolate = TRUE, data = topo[h >= 0]) +
#'     geom_contour(aes(z = h), breaks = 0, color = "black") +
#'     geom_streamline(aes(dx = -dx, dy = -dy), L = 10, skip = 3, arrow = NULL,
#'                     color = "#4658BD") +
#'     coord_quickmap()
#'  }
#'
#' @family ggplot2 helpers
#' @export
geom_streamline <-  function(mapping = NULL, data = NULL,
                             stat = "streamline", position = "identity",
                             ...,
                             L = 5,
                             res = 1,
                             S = NULL,
                             dt = NULL,
                             skip = 1,
                             skip.x = skip,
                             skip.y = skip,
                             n = NULL,
                             nx = n,
                             ny = n,
                             jitter = 1,
                             jitter.x = jitter,
                             jitter.y = jitter,
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
    ggplot2::layer(
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
            dt = dt,
            S = S,
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            skip.x = skip.x,
            skip.y = skip.y,
            nx = nx,
            ny = ny,
            jitter.x = jitter.x,
            jitter.y = jitter.y,
            ...
        )
    )
}

#' @export
#' @rdname geom_streamline
stat_streamline <- function(mapping = NULL, data = NULL,
                            geom = "streamline", position = "identity",
                            ...,
                            L = 5,
                            res = 1,
                            S = NULL,
                            dt = NULL,
                            skip = 1,
                            skip.x = skip,
                            skip.y = skip,
                            n = NULL,
                            nx = n,
                            ny = n,
                            jitter = 1,
                            jitter.x = jitter,
                            jitter.y = jitter,
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
            dt = dt,
            S = S,
            arrow = arrow,
            lineend = lineend,
            na.rm = na.rm,
            skip.x = skip.x,
            skip.y = skip.y,
            nx = nx,
            ny = ny,
            jitter.x = jitter.x,
            jitter.y = jitter.y,
            ...
        )
    )
}

StatStreamline <- ggplot2::ggproto("StatStreamline", ggplot2::Stat,
    required_aes = c("x", "y", "dx", "dy"),
    setup_params = function(data, params) {

        # M <- with(data, max(Mag(dx, dy), na.rm = T))
        m <- with(data, mean(Mag(dx, dy), na.rm = T))  # No me gustaaaa
        r <- min(ggplot2::resolution(data$x, zero = FALSE),
                 ggplot2::resolution(data$y, zero = FALSE))

        if (is.null(params$dt)) params$dt <- r/m/params$res

        if (is.null(params$S)) params$S <- ceiling(params$L/params$dt/m)
        if (params$S == 1) warning("performing only 1 integration step, please consider increasing the resolution")

        return(params)
    },
    compute_group = function(data, scales, dt = 0.1, S = 3, skip.x = 1,
                             skip.y = 1, nx = 10, ny  = 10, jitter.x = 1,
                             jitter.y = 1,
                             L = NULL, res = NULL) {

        data <- streamline.f(data, dt = dt, S = S, skip.x = skip.x,
                             skip.y = skip.y, nx = nx, ny = ny, jitter.x = jitter.x,
                             jitter.y = jitter.y)

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
                       ny = NULL, jitter.x = 1, jitter.y = 1) {

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
    range.x <- range(field$x)
    range.y <- range(field$y)
    # Build grid
    if (is.null(nx)) {
        x <- JumpBy(unique(field$x), skip.x + 1)
    } else {
        x <- seq(range.x[1], range.x[2], length.out = nx)
    }
    if (is.null(ny)) {
        y <- JumpBy(unique(field$y), skip.y + 1)
    } else {
        y <- seq(range.y[1], range.y[2], length.out = ny)
    }
    rx <- ggplot2::resolution(field$x, zero = FALSE)
    ry <- ggplot2::resolution(field$y, zero = FALSE)

    points <- as.data.table(expand.grid(x = x, y = y))

    set.seed(42)
    points[, x := x + rnorm(.N, 0, rx)*jitter.x]
    points[, y := y + rnorm(.N, 0, ry)*jitter.y]
    points[, x := ifelse(x > range.x[2], range.x[2], x)]
    points[, x := ifelse(x < range.x[1], range.x[1], x)]

    points[, y := ifelse(y > range.y[2], range.y[2], y)]
    points[, y := ifelse(y < range.y[1], range.y[1], y)]


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

#' @importFrom memoise memoise
streamline.f <- memoise::memoise(streamline)
