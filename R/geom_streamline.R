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
#' @param start optional list/data.frame with x and y columns giving the starting
#' locations for integration.
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
#' # get cute "drops". It's important to note that after_stat(dx) (the calculated variable)
#' # is NOT the same as dx (from the data).
#' ggplot(geopotential, aes(lon, lat)) +
#'     geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v), alpha = after_stat(step),
#'                         color = sqrt(after_stat(dx^2) + after_stat(dy^2)),
#'                         size = after_stat(step)),
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
geom_streamline <- function(
  mapping = NULL,
  data = NULL,
  stat = "streamline",
  position = "identity",
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
  start = NULL,
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
  arrow = grid::arrow(
    arrow.angle,
    grid::unit(arrow.length, "lines"),
    ends = arrow.ends,
    type = arrow.type
  ),
  lineend = "butt",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
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
      start = start,
      ...
    )
  )
}

#' @export
#' @rdname geom_streamline
stat_streamline <- function(
  mapping = NULL,
  data = NULL,
  geom = "streamline",
  position = "identity",
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
  start = NULL,
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
  arrow = grid::arrow(
    arrow.angle,
    grid::unit(arrow.length, "lines"),
    ends = arrow.ends,
    type = arrow.type
  ),
  lineend = "butt",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
) {
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
      start = start,
      ...
    )
  )
}

#' @rdname geom_streamline
#' @usage NULL
#' @format NULL
#' @export
StatStreamline <- ggplot2::ggproto(
  "StatStreamline",
  ggplot2::Stat,
  required_aes = c("x", "y", "dx", "dy"),
  setup_params = function(data, params) {
    # M <- with(data, max(Mag(dx, dy), na.rm = T))
    m <- with(data, mean(Mag(dx, dy), na.rm = TRUE)) # No me gustaaaa
    r <- min(
      ggplot2::resolution(data$x, zero = FALSE),
      ggplot2::resolution(data$y, zero = FALSE)
    )

    if (is.null(params$dt)) {
      params$dt <- r / m / params$res
    }

    if (is.null(params$S)) {
      params$S <- ceiling(params$L / params$dt / m / 2)
    }
    if (params$S == 1) {
      warningf(
        "Performing only 1 integration step, please consider increasing the resolution."
      )
    }

    return(params)
  },
  setup_data = function(data, params) {
    data$dx[is.na(data$dx)] <- 0
    data$dy[is.na(data$dy)] <- 0
    data
  },
  compute_group = function(
    data,
    scales,
    dt = 0.1,
    S = 3,
    skip.x = 1,
    skip.y = 1,
    nx = 10,
    ny = 10,
    jitter.x = 1,
    jitter.y = 1,
    xwrap = NULL,
    ywrap = NULL,
    min.L = 0,
    L = NULL,
    res = NULL,
    no.cache = FALSE,
    start = NULL
  ) {
    if (no.cache == TRUE) {
      memoise::forget(streamline.f)
    }
    data <- streamline.f(
      data,
      dt = dt,
      S = S,
      skip.x = skip.x,
      skip.y = skip.y,
      nx = nx,
      ny = ny,
      jitter.x = jitter.x,
      jitter.y = jitter.y,
      xwrap = xwrap,
      ywrap = ywrap,
      start = start
    )

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
GeomStreamline <- ggplot2::ggproto(
  "GeomStreamline",
  ggplot2::GeomPath,
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  ),
  rename_size = TRUE,
  non_missing_aes = "size",
  draw_panel = function(
    data,
    panel_params,
    coord,
    arrow = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 1,
    na.rm = FALSE
  ) {
    if (!anyDuplicated(data$group)) {
      messagef(
        "%s: Each group consists of only one observation.\nDo you need to adjust the group aesthetic?",
        "geom_path"
      )
    }
    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- ggplot2::coord_munch(coord, data, panel_params)

    # For older versions of ggplot2
    if (is.null(data$linewidth)) {
      data$linewidth <- data$size
    }

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) {
      return(ggplot2::zeroGrob())
    }

    # Work out whether we should use lines or segments
    attr <- plyr::ddply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      data.frame(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique(df[, c(
          "alpha",
          "colour",
          "linewidth",
          "linetype"
        )])) ==
          1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stopf(
        "%s: If you are using dotted or dashed lines, colour, linewidth and linetype must be constant over the line.",
        "geom_streamline",
        call. = FALSE
      )
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)

    if (!constant) {
      if (!is.null(arrow)) {
        mult <- end & munched$end
        mult <- mult[!start]
        if ("simpleUnit" %in% class(grid::unit(1, "mm"))) {
          arrow$length <- as.numeric(mult) * arrow$length[1]
        } else {
          arrow$length <- grid::unit(
            as.numeric(arrow$length)[1] * mult,
            attr(arrow$length, "unit")
          )
        }
      }
      grid::segmentsGrob(
        munched$x[!end],
        munched$y[!end],
        munched$x[!start],
        munched$y[!start],
        default.units = "native",
        arrow = arrow,
        gp = grid::gpar(
          col = scales::alpha(munched$colour, munched$alpha)[!end],
          fill = scales::alpha(munched$colour, munched$alpha)[!end],
          lwd = munched$linewidth[!end] * .pt,
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
          arrow$length <- mult * arrow$length[1]
        } else {
          arrow$length <- grid::unit(
            as.numeric(arrow$length)[1] * mult,
            attr(arrow$length, "unit")
          )
        }
      }
      grid::polylineGrob(
        munched$x,
        munched$y,
        id = id,
        default.units = "native",
        arrow = arrow,
        gp = grid::gpar(
          col = scales::alpha(munched$colour, munched$alpha)[start],
          fill = scales::alpha(munched$colour, munched$alpha)[start],
          lwd = munched$linewidth[start] * .pt,
          lty = munched$linetype[start],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
    }
  }
)


create_start <- function(
  field,
  skip.x = 1,
  skip.y = 1,
  nx = NULL,
  ny = NULL,
  jitter.x = 1,
  jitter.y = 1
) {
  rx <- ggplot2::resolution(as.numeric(field$x), zero = FALSE)
  ry <- ggplot2::resolution(as.numeric(field$y), zero = FALSE)
  range.x <- range(field$x)
  range.y <- range(field$y)

  if (is.null(nx)) {
    xs <- JumpBy(unique(field$x), skip.x + 1)
  } else {
    xs <- seq(range.x[1], range.x[2], length.out = nx)
  }
  if (is.null(ny)) {
    ys <- JumpBy(unique(field$y), skip.y + 1)
  } else {
    ys <- seq(range.y[1], range.y[2], length.out = ny)
  }

  if ((is.null(nx) && is.null(ny))) {
    points <- data.table::as.data.table(field[
      x %in% xs & y %in% ys,
      .(x = x, y = y)
    ])
  } else {
    points <- data.table::as.data.table(expand.grid(x = xs, y = ys))
  }

  withr::with_seed(42, {
    points[, x := x + rnorm(.N, 0, rx) * jitter.x]
    points[, y := y + rnorm(.N, 0, ry) * jitter.y]
  })
  return(points)
}

#' @importFrom stats rnorm
#' @importFrom data.table %between%
streamline.f <- function(
  field,
  geometry = c("cartesian", "spherical", "cylindrical_x", "cylindrical_y"),
  dt = 0.1,
  S = 3,
  skip.x = 1,
  skip.y = 1,
  nx = NULL,
  ny = NULL,
  jitter.x = 1,
  jitter.y = 1,
  start = NULL,
  xwrap = NULL,
  ywrap = NULL
) {
  geometry <- "spherical"
  field <- data.table::copy(data.table::as.data.table(field))
  is.grid <- with(field, .is.regular_grid(x, y))

  if (!is.grid) {
    stopf("'x' and 'y' do not define a regular grid.")
  }

  data.table::setorder(field, x, y)

  field <- field[!is.na(dx) & !is.na(dy)]

  range.x <- range(field$x)
  range.y <- range(field$y)

  matrix <- .tidy2matrix(field, x ~ y, value.var = "dx", fill = 0)
  dx.field <- list(
    x = matrix$rowdims$x,
    y = matrix$coldims$y,
    z = matrix$matrix
  )

  matrix <- .tidy2matrix(field, x ~ y, value.var = "dy", fill = 0)
  dy.field <- list(
    x = matrix$rowdims$x,
    y = matrix$coldims$y,
    z = matrix$matrix
  )

  fold_position <- switch(
    geometry[1],
    cartesian = identity,
    spherical = function(xy) {
      fold_sphere(xy, lon_range = 360)
    },
    cylindrical_x = function(xy) {
      xy$x <- fold_cylinder(xy$x, lon_range = 360)
      return(xy)
    },
    cylindrical_y = function(xy) {
      xy$y <- fold_cylinder(xy$y, lon_range = 360)
      return(xy)
    }
  )

  fold_delta <- switch(
    geometry[1],
    cartesian = function(delta, xy) {
      delta
    },
    spherical = function(delta, xy) {
      delta$dy <- fold_sphere_delta(delta$dy, xy$y)
      return(delta)
    },
    cylindrical_x = function(delta, xy) {
      delta
    },
    cylindrical_y = function(delta, xy) {
      delta
    }
  )

  force.fun <- function(X) {
    X_fold <- with(
      fold_position(list(x = X[, 1], y = X[, 2])),
      cbind(x, y)
    )

    dx <- interpolate_locations(dx.field, X_fold)
    dy <- interpolate_locations(dy.field, X_fold)

    delta <- fold_delta(list(dx = dx, dy = dy), list(x = X[, 1], y = X[, 2]))

    return(cbind(dx = delta$dx, dy = delta$dy))
  }

  if (is.null(start)) {
    # Create starting points
    start <- create_start(
      field,
      skip.x = skip.x,
      skip.y = skip.y,
      nx = nx,
      ny = ny,
      jitter.x = jitter.x,
      jitter.y = jitter.y
    )
  }

  if (!all(c("x", "y") %in% names(start))) {
    stopf("'start' needs to have x and y elements.")
  }
  if (!data.table::uniqueN(lengths(start)) == 1) {
    stopf("'x' and 'y' elements in 'start' need to be of the same length.")
  }
  if (!all(vapply(start, is.numeric, logical(1)))) {
    stopf("'x' and 'y' elements in 'start' need to be numeric.")
  }

  points <- data.table::as.data.table(start)

  points[, group := interaction(1:.N, field$group[1])]
  points[, piece := "1"]

  points[,
    c("x", "y") := fold_position(list(x = x, y = y))
  ]

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

    points_forw[,
      c("x", "y") := runge_kutta4(
        x,
        y,
        force.fun,
        dt
      )
    ]

    points_forw[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]

    points_back[,
      c("x", "y") := runge_kutta4(
        x,
        y,
        force.fun,
        -dt
      )
    ]

    points_back[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]

    points_forw <- points_forw[!is.na(dx) & !is.na(dy)]
    points_back <- points_back[!is.na(dx) & !is.na(dy)]

    accum_forw[[s]] <- points_forw
    accum_back[[S - s + 1]] <- points_back
  }

  points <- data.table::rbindlist(c(accum_back, list(points), accum_forw)) # se puede optimizar prealocando

  if (geometry %in% c("spherical")) {
    points <- points[,
      fold_streamline(x, y),
      by = .(group)
    ]
  }

  points[, c("dx", "dy") := as.list(force.fun(cbind(x, y)))]

  points[, end := TRUE]

  points[, step := seq(1, .N), by = .(group)]

  points[, group := interaction(group, piece, end)]
  points[, line := group]

  return(points[, .(x, y, group, piece, end, step, dx, dy, line)])
}


cut_streamlines <- function(
  x,
  y,
  piece,
  x_range = c(0, 360)
) {
  x2 <- fold_cylinder(x, lon_range = 360)
  lon_diff <- c(0, diff(x2))
  cut_points <- which(abs(lon_diff) > diff(x_range) / 2)
  if (length(cut_points) == 0) {
    return(list(x = x, y = y, piece = piece))
  }
  n_pieces <- length(cut_points) + 1
  piece2 <- rep(
    seq_len(n_pieces),
    c(
      cut_points[1],
      diff(cut_points) + 2,
      length(x) - cut_points[length(cut_points)] + 2
    )
  )
  piece <- paste0(piece, "_", piece2)

  offset <- 0
  for (cut in cut_points) {
    cut_where <- cut + offset
    if (lon_diff[cut] > 0) {
      # going left to right
      x_border <- x_range[1] + x[cut - 1] - x2[cut_where - 1]

      x2 <- c(
        x2[1:(cut_where - 1)],
        x_range[1],
        x_range[2],
        x2[cut_where:length(x2)]
      )
    } else {
      # going right to left
      x_border <- x_range[1] + x[cut] - x2[cut_where]

      x2 <- c(
        x2[1:(cut_where - 1)],
        x_range[2],
        x_range[1],
        x2[cut_where:length(x2)]
      )
    }

    #interpolate y at border
    slope <- (y[cut_where] - y[cut_where - 1]) /
      (x[cut] - x[cut - 1])
    y_border <- y[cut_where - 1] +
      slope * (x_border - x[cut - 1])

    y <- c(
      y[1:(cut_where - 1)],
      y_border,
      y_border,
      y[cut_where:length(y)]
    )

    offset <- offset + 2
  }

  list(x = x2, y = y, piece = piece)
}
