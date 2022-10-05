#' Streamlines
#'
#' Streamlines are paths that are always tangential to a vector field. In the
#' case of a steady field, it's identical to the path of a massless particle that
#' moves with the "flow".
#'
#' @inheritParams geom_vector
#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_contour2
#' @param L, typical length of a streamline in x and y units
#' @param min.L minimum length of segments to show
#' @param res, resolution parameter (higher numbers increases the resolution)
#' @param S optional numeric number of timesteps for integration
#' @param dt optional numeric size "timestep" for integration
#' @param n,nx,ny optional numeric indicating the number of points to draw in the
#' x and y direction (replaces `skip` if not `NULL`)
#' @param jitter,jitter.x,jitter.y amount of jitter of the starting points
#' @param xwrap,ywrap vector of length two used to wrap the circular dimension.
#'
#' @details
#' Streamlines are computed by simple integration with a forward Euler method.
#' By default, `stat_streamline()` computes `dt` and `S` from `L`, `res`,
#' the resolution of the grid and the mean magnitude of the field. `S` is
#' then defined as the number of steps necessary to make a streamline of length
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
#' Missing values are not permitted and the field must be defined on a
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
#' @section Computed variables:
#' \describe{
#'  \item{step}{step in the simulation}
#'  \item{dx}{dx at each location of the streamline}
#'  \item{dy}{dy at each location of the streamline}
#'  }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(ggplot2)
#' data(geopotential)
#'
#' geopotential <- copy(geopotential)[date == date[1]]
#' geopotential[, gh.z := Anomaly(gh), by = .(lat)]
#' geopotential[, c("u", "v") := GeostrophicWind(gh.z, lon, lat)]
#'
#' (g <- ggplot(geopotential, aes(lon, lat)) +
#'     geom_contour2(aes(z = gh.z), xwrap = c(0, 360)) +
#'     geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v)), L = 60,
#'                     xwrap = c(0, 360)))
#'
#' # The circular parameter is particularly important for polar coordinates
#' g + coord_polar()
#'
#' # If u and v are not converted into degrees/second, the resulting
#' # streamlines have problems, specially near the pole.
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.z)) +
#'     geom_streamline(aes(dx = u, dy = v), L = 50)
#'
#' # The step variable can be mapped to size or alpha to
#' # get cute "drops". It's important to note that ..dx.. (the calculated variable)
#' # is NOT the same as dx (from the data).
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v), alpha = ..step..,
#'                         color = sqrt(..dx..^2 + ..dy..^2), size = ..step..),
#'                         L = 40, xwrap = c(0, 360), res = 2, arrow = NULL,
#'                         lineend = "round") +
#'     scale_size(range = c(0, 0.6))
#'
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
                             min.L = 0,
                             res = 1,
                             S = NULL,
                             dt = NULL,
                             xwrap = NULL,
                             ywrap = NULL,
                             skip = 1,
                             skip.x = skip,
                             skip.y = skip,
                             n = NULL,
                             nx = n,
                             ny = n,
                             jitter = 1,
                             jitter.x = jitter,
                             jitter.y = jitter,
                             arrow.angle = 6,
                             arrow.length = 0.5,
                             arrow.ends = "last",
                             arrow.type = "closed",
                             arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"),
                                                 ends = arrow.ends, type = arrow.type),
                             lineend = "butt",
                             na.rm = TRUE,
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
            min.L = min.L,
            res = res,
            xwrap = xwrap,
            ywrap = ywrap,
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
                            min.L = 0,
                            res = 1,
                            S = NULL,
                            dt = NULL,
                            xwrap = NULL,
                            ywrap = NULL,
                            skip = 1,
                            skip.x = skip,
                            skip.y = skip,
                            n = NULL,
                            nx = n,
                            ny = n,
                            jitter = 1,
                            jitter.x = jitter,
                            jitter.y = jitter,
                            arrow.angle = 6,
                            arrow.length = 0.5,
                            arrow.ends = "last",
                            arrow.type = "closed",
                            arrow = grid::arrow(arrow.angle, grid::unit(arrow.length, "lines"),
                                                ends = arrow.ends, type = arrow.type),
                            lineend = "butt",
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatStreamline,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            L = L,
            min.L = min.L,
            res = res,
            dt = dt,
            S = S,
            xwrap = xwrap,
            ywrap = ywrap,
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

#' @rdname geom_streamline
#' @usage NULL
#' @format NULL
#' @export
StatStreamline <- ggplot2::ggproto("StatStreamline", ggplot2::Stat,
  required_aes = c("x", "y", "dx", "dy"),
  setup_params = function(data, params) {
      # M <- with(data, max(Mag(dx, dy), na.rm = T))
      m <- with(data, mean(Mag(dx, dy), na.rm = TRUE))  # No me gustaaaa
      r <- min(ggplot2::resolution(data$x, zero = FALSE),
               ggplot2::resolution(data$y, zero = FALSE))

      if (is.null(params$dt)) params$dt <- r/m/params$res

      if (is.null(params$S)) params$S <- ceiling(params$L/params$dt/m/2)
      if (params$S == 1) {
          warningf("Performing only 1 integration step, please consider increasing the resolution.")
      }

      return(params)
  },
  compute_group = function(data, scales, dt = 0.1, S = 3, skip.x = 1,
                           skip.y = 1, nx = 10, ny  = 10, jitter.x = 1,
                           jitter.y = 1, xwrap = NULL, ywrap = NULL,
                           min.L = 0,
                           L = NULL, res = NULL,
                           no.cache = FALSE) {

      if (no.cache == TRUE) memoise::forget(streamline.f)
      data <- streamline.f(data, dt = dt, S = S, skip.x = skip.x,
                           skip.y = skip.y, nx = nx, ny = ny, jitter.x = jitter.x,
                           jitter.y = jitter.y, xwrap = xwrap, ywrap = ywrap)

      distance <- data[, .(dx = diff(x), dy = diff(y)), by = line]
      distance <- distance[, .(distance = sum(sqrt(dx^2 + dy^2))), by = line]
      keep <- distance[distance >= min.L, line]

      return(data.table::setDF(data[line %in% keep]))
  }
)

#' @rdname geom_streamline
#' @usage NULL
#' @format NULL
#' @export
GeomStreamline <- ggplot2::ggproto("GeomStreamline", ggplot2::GeomPath,
  default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 1,
                        na.rm = FALSE) {
      if (!anyDuplicated(data$group)) {
          messagef("%s: Each group consists of only one observation.\nDo you need to adjust the group aesthetic?", "geom_path")
      }

      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]
      munched <- ggplot2::coord_munch(coord, data, panel_params)

      # Silently drop lines with less than two points, preserving order
      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
      munched <- munched[rows >= 2, ]
      if (nrow(munched) < 2) return(ggplot2::zeroGrob())

      # Work out whether we should use lines or segments
      attr <- plyr::ddply(munched, "group", function(df) {
          linetype <- unique(df$linetype)
          data.frame(
              solid = identical(linetype, 1) || identical(linetype, "solid"),
              constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
          )
      })
      solid_lines <- all(attr$solid)
      constant <- all(attr$constant)
      if (!solid_lines && !constant) {
        stopf("%s: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line.",
                      "geom_streamline", call. = FALSE)
      }

      # Work out grouping variables for grobs
      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)

      if (!constant) {
          if (!is.null(arrow)) {
              mult <- end&munched$end
              mult <- mult[!start]
              if ("simpleUnit" %in% class(grid::unit(1, "mm"))) {
                  arrow$length <- mult*arrow$length[1]
              } else {
                  arrow$length <- grid::unit(as.numeric(arrow$length)[1]*mult,
                                       attr(arrow$length, "unit"))
              }
          }
          grid::segmentsGrob(
              munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
              default.units = "native", arrow = arrow,
              gp = grid::gpar(
                  col = scales::alpha(munched$colour, munched$alpha)[!end],
                  fill = scales::alpha(munched$colour, munched$alpha)[!end],
                  lwd = munched$size[!end] * .pt,
                  lty = munched$linetype[!end],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      } else {
          id <- match(munched$group, unique(munched$group))

          if (!is.null(arrow)) {
              mult <- as.numeric(munched$end)[start]
              if ("simpleUnit" %in% class(grid::unit(1, "mm"))) {
                  arrow$length <- mult*arrow$length[1]
              } else {
                  arrow$length <- grid::unit(as.numeric(arrow$length)[1]*mult,
                                       attr(arrow$length, "unit"))
              }
          }
          grid::polylineGrob(
              munched$x, munched$y, id = id,
              default.units = "native", arrow = arrow,
              gp = grid::gpar(
                  col = scales::alpha(munched$colour, munched$alpha)[start],
                  fill = scales::alpha(munched$colour, munched$alpha)[start],
                  lwd = munched$size[start] * .pt,
                  lty = munched$linetype[start],
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre
              )
          )
      }
  }
)


#' @importFrom stats rnorm
#' @importFrom data.table %between%
streamline.f <- function(field, dt = 0.1, S = 3, skip.x = 1, skip.y = 1, nx = NULL,
                       ny = NULL, jitter.x = 1, jitter.y = 1, xwrap = NULL,
                       ywrap = NULL) {
    field <- data.table::copy(data.table::as.data.table(field))

    is.grid <- with(field, .is.regular_grid(x, y))

    if (!is.grid) {
      stopf("'x' and 'y' do not define a regular grid.")
    }

    data.table::setorder(field, x, y)

    circ.x <- !is.null(xwrap)
    circ.y <- !is.null(ywrap)

    if (circ.x) field <- suppressWarnings(WrapCircular(field, "x", xwrap))
    if (circ.y) field <- suppressWarnings(WrapCircular(field, "y", ywrap))

    field <- field[!is.na(dx) & !is.na(dy)]

    rx <- ggplot2::resolution(as.numeric(field$x), zero = FALSE)
    ry <- ggplot2::resolution(as.numeric(field$y), zero = FALSE)
    range.x <- range(field$x)
    range.y <- range(field$y)

    matrix <- .tidy2matrix(field, x ~ y, value.var = "dx", fill = 0)
    dx.field <- list(x = matrix$rowdims$x,
                     y = matrix$coldims$y,
                     z = matrix$matrix)

    matrix <- .tidy2matrix(field, x ~ y, value.var = "dy", fill = 0)
    dy.field <- list(x = matrix$rowdims$x,
                     y = matrix$coldims$y,
                     z = matrix$matrix)

    force.fun <- function(X) {
        X[, 1] <- .fold(X[, 1], 1, range.x, circ.x)[[1]]
        X[, 2] <- .fold(X[, 2], 1, range.y, circ.y)[[1]]

        dx <- interpolate_locations(dx.field, X)
        dy <- interpolate_locations(dy.field, X)
        return(cbind(dx = dx, dy = dy))
    }

    # Build grid
    if (is.null(nx)) {
        xs <- JumpBy(dx.field$x, skip.x + 1)
    } else {
        xs <- seq(range.x[1], range.x[2], length.out = nx)
    }
    if (is.null(ny)) {
        ys <- JumpBy(dx.field$y, skip.y + 1)
    } else {
        ys <- seq(range.y[1], range.y[2], length.out = ny)
    }

    if ((is.null(nx) && is.null(ny))) {
        points <- data.table::as.data.table(field[x %in% xs & y %in% ys, .(x = x, y = y)])
    } else {
        points <- data.table::as.data.table(expand.grid(x = xs, y = ys))
    }

    set.seed(42)
    points[, x := x + rnorm(.N, 0, rx)*jitter.x]
    points[, y := y + rnorm(.N, 0, ry)*jitter.y]

    points[, group := interaction(1:.N, field$group[1])]
    points[, piece := 1]
    points[, step := 0]
    points[, end := FALSE]

    # Muy poco elegante, pero bueno...
    if (circ.x == TRUE){
        points[, x := .fold(x, 1, range.x, circ.x)[[1]]]
    } else {
        points[, x := ifelse(x > range.x[2], range.x[2], x)]
        points[, x := ifelse(x < range.x[1], range.x[1], x)]
    }

    if (circ.y == TRUE){
        points[, y := .fold(y, 1, range.y, circ.y)[[1]]]
    } else {
        points[, y := ifelse(y > range.y[2], range.y[2], y)]
        points[, y := ifelse(y < range.y[1], range.y[1], y)]
    }

    as.list.matrix <- function(x, ...) {
        list(x[, 1], x[, 2])
    }

    points[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]
    points <- points[abs(dx) + abs(dy) != 0 & !is.na(dx) & !is.na(dy)]
    points[, sign := 1]

    points_forw <- data.table::copy(points)
    points_forw[, sign := 1]
    points_back <- data.table::copy(points)
    points_back[, sign := -1]

    accum_forw <- vector(mode = "list", length = S)
    accum_back <- vector(mode = "list", length = S)
    # Integration
    for (s in 1:S) {
        points_forw <- points_forw[dx + dy != 0]
        points_back <- points_back[dx + dy != 0]

        points_forw[, c("x", "y") := runge_kutta4(x, y, force.fun, dt, piece, list(range.x, range.y), c(circ.x, circ.y))]
        points_forw[, step := s]
        points_forw[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]

        points_back[, c("x", "y") := runge_kutta4(x, y, force.fun, -dt, piece, list(range.x, range.y), c(circ.x, circ.y))]
        points_back[, step := -s]
        points_back[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]

        points_forw <- points_forw[!is.na(dx) & !is.na(dy)]
        points_back <- points_back[!is.na(dx) & !is.na(dy)]

        accum_forw[[s]] <- points_forw
        accum_back[[S - s + 1]] <- points_back
    }

    # accum_back <- list()
    points <- data.table::rbindlist(c(accum_back, list(points), accum_forw)) # se puede optimizar prealocando
    # points[, step := step - min(step)]
    # Empalmo los pieces que pasan de un lado
    # al otro del dominio.
    range.select <- function(sign, range) {
        ifelse(sign == 1, range[2], range[1])
    }
    points[, step2 := step]
    if (circ.x == TRUE) {
        points <- points[, .approx_order(x, y, range.x), by = group]
        points[, piece := as.numeric(data.table::rleid(x %between% range.x)), by = group]
        points[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]
        points[, step := seq_along(x), by = group]
        points[, x := .fold(x, 1, range.x, circ.x)[[1]]]
    }


    if (circ.y == TRUE) {
        points <- points[, .approx_order(y, x, range.y), by = group]
        points[, piece := as.numeric(data.table::rleid(y %between% range.y)), by = group]
        points[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]
        points[, step := seq_along(y), by = group]
        points[, y := .fold(y, 1, range.y, circ.y)[[1]]]
    }

    # Me fijo si ese piece tiene el final.
    # Esto luego va al geom que decide si ponerle flecha o no.
    points[, end := seq_len(.N) < .N/2, by = .(group, piece)]
    points_last <- points[end == FALSE, ]
    points_first <- rbind(points[end == TRUE, ],
                          points_last[, head(.SD, 1), by = .(group, piece)])
    points_first[, end := TRUE]
    points <- rbind(points_first, points_last)

    points[, group := interaction(group, piece, end)]
    points[, line := group]

    return(points[, .(x, y, group, piece, end, step, dx, dy, line)])
}



.fold <- function(x, piece, range, circular = TRUE) {
    if (circular) {
        R <- diff(range)
        piece <- ifelse(x > range[2], piece + 1, piece)
        x <- ifelse(x > range[2], x - R, x)

        piece <- ifelse(x < range[1], piece + 1, piece)
        x <- ifelse(x < range[1], x + R, x)
    } else {
        x <- ifelse(x > range[2], NA, x)
        x <- ifelse(x < range[1], NA, x)
    }
    return(list(x, piece))
}


#
# xbk <- x
# ybk <- y
# extra.x <- c(0, 360)
.approx_order <- function(x, y, extra.x) {
    rx <- ggplot2::resolution(x, zero = FALSE)
    extra.x <- c(extra.x - rx/100000, extra.x + rx/100000)
    for (i in seq_along(extra.x)) {
        ind <- which(diff(x < extra.x[i]) != 0) + 1
        if (length(ind) != 0) {
            val <- rep(extra.x[i], length(ind))

            new_x <- vector(mode = "numeric", length(x) + length(val))
            new_y <- new_x
            new_x[-ind] <- x
            new_x[ind] <- val

            new_y[-ind] <- y
            new_y[ind] <- y[ind-1] +  (extra.x[i] - x[ind-1])  * (diff(y)/diff(x))[ind - 1]
            x <- new_x
            y <- new_y
        }
    }

    return(list(x = x, y = y))
}
