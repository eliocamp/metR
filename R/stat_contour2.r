#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_contour_fill
#' @param breaks One of:
#'   - A numeric vector of breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
#' @param global.breaks Logical indicating whether `breaks` should be computed for the whole
#' data or for each grouping.
#' @param kriging Logical indicating whether to perform ordinary kriging before contouring.
#' Use this if you want to use contours with irregularly spaced data.
#'
#' @export
#' @section Computed variables:
#' \describe{
#'  \item{level}{height of contour}
#' }
#' @rdname geom_contour2
#' @family ggplot2 helpers
stat_contour2 <- function(mapping = NULL, data = NULL,
                          geom = "contour2", position = "identity",
                          ...,
                          breaks = MakeBreaks(),
                          bins = NULL,
                          binwidth = NULL,
                          kriging = FALSE,
                          global.breaks = TRUE,
                          na.rm = FALSE,
                          na.fill = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    .check_wrap_param(list(...))
  ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatContour2,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            na.fill = na.fill,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            global.breaks = global.breaks,
            kriging = kriging,
            ...
        )
    )
}

#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
StatContour2 <- ggplot2::ggproto("StatContour2", ggplot2::Stat,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(order = ..level..),
  setup_params = function(data, params) {
    if (is.null(params$global) || isTRUE(params$global.breaks)) {
      params$breaks <- setup_breaks(data,
                                    breaks = params$breaks,
                                    bins = params$bins,
                                    binwidth = params$binwidth)
    }
    return(params)
  },
  compute_layer = function(self, data, params, layout) {
      ggplot2:::check_required_aesthetics(
          self$required_aes,
          c(names(data), names(params)),
          ggplot2:::snake_class(self)
      )

      # Trim off extra parameters
      params <- params[intersect(names(params), self$parameters())]

      args <- c(list(data = quote(data), scales = quote(scales)), params)
      plyr::ddply(data, "PANEL", function(data) {
          scales <- layout$get_scales(data$PANEL[1])
          tryCatch(do.call(self$compute_panel, args), error = function(e) {
              warningf("Computation failed in `%s()`:\n %s",
                               ggplot2:::snake_class(self), e$message,
                      call. = FALSE)
              data.frame()
          })
      })
  },
  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = scales::fullseq, complete = TRUE,
                           na.rm = FALSE, circular = NULL, xwrap = NULL,
                           ywrap = NULL, na.fill = FALSE, global.breaks = TRUE,
                           proj = NULL, kriging = FALSE) {
    if (isFALSE(global.breaks)) {
      breaks <- setup_breaks(data,
                             breaks = breaks,
                             bins = bins,
                             binwidth = binwidth)
    }

    data.table::setDT(data)

    data <- data[!(is.na(y) | is.na(x)), ]

    if (isFALSE(na.fill)) {
      data <- data[!is.na(z), ]
    }


    nx <- data[, data.table::uniqueN(x), by = y]$V1
    ny <- data[, data.table::uniqueN(y), by = x]$V1

    complete.grid <- abs(max(nx) - min(nx)) == 0 & abs(max(ny) - min(ny)) == 0

    if (complete.grid == FALSE) {
      if (complete == FALSE) {
        warningf("The data must be a complete regular grid.", call. = FALSE)
        return(data.frame())
      } else {
        # data <- setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
        data <- .complete(data, x, y)
      }
    }

    data <- .impute_data.m(data, na.fill)


    if (kriging) {
      check_packages("kriging", "kriging")

      pixels <- 40
      data <- try(with(data, setNames(kriging::kriging(x, y, z, pixels = pixels)$map,
                                      c("x", "y", "z"))), silent = TRUE)
      if (inherits(data, "try-error")) {
        warningf("kriging failed. Perhaps the number of points is too small.")
        return(data.frame())
      }

      data.table::setDT(data)
    }

    if (!is.null(xwrap)) {
      data <- suppressWarnings(WrapCircular(data, "x", xwrap))
    }
    if (!is.null(ywrap)) {
      data <- suppressWarnings(WrapCircular(data, "y", ywrap))
    }


    data.table::setDF(data)
    contours <- data.table::as.data.table(.contour_lines(data, breaks, complete = complete))

    if (length(contours) == 0) {
      warningf("Not possible to generate contour data.", call. = FALSE)
      return(data.frame())
    }

    # contours[, start := 1:.N %in% 1 , by = .(piece, group)]
    # contours <- contours[, unique(.SD), by = .(group, piece)]
    # contours[, start := NULL]
    contours <- .order_contour.m(contours, data.table::setDT(data))

    if (!is.null(proj)) {
      if (is.function(proj)) {
        contours <- proj(contours)
      } else {
        if (is.character(proj)) {
          if (!requireNamespace("proj4", quietly = TRUE)) {
            stopf("Projection requires the proj4 package. Install it with 'install.packages(\"proj4\")'.")
          }
          contours <- data.table::copy(contours)[, c("x", "y") := proj4::project(list(x, y), proj,
                                                                                 inverse = TRUE)][]

        }
      }
    }

    return(contours)
  }
)


.complete <- function(data, ...) {
  l <-  match.call(expand.dots = FALSE)$`...`
  coord <- with(data, do.call(data.table::CJ, c(l, list(unique = TRUE))))

  data[coord, on = as.character(l), allow.cartesian = TRUE]

}

.order_contour <- function(contours, data) {
  data <- data.table::copy(data)
    contours <- data.table::copy(contours)
    x.data <- unique(data$x)
    x.data <- x.data[order(x.data)]
    x.N <- length(x.data)
    y.data <- unique(data$y)
    y.data <- y.data[order(y.data)]
    y.N <- length(y.data)

    contours[, c("dx", "dy") := .(c(diff(x), NA), c(diff(y), NA)), by = group]

    segments <- contours[dx != 0 & dy != 0]

    segments[, c("x.axis", "y.axis") := .(x %in% x.data, y %in% y.data), by = group]

    # x axis
    x.axis <- segments[x.axis == TRUE]
    x.axis[, x.axis := NULL]   # remove annoying column
    x.axis[, y.d := .second(y.data, y), by = .(group, y)]  # select 2nd closest data point
    x.axis[, m := y - y.d]

    x.axis <- data[, .(x, y.d = y, z)][x.axis, on = c("x", "y.d")]  # get z column
    x.axis <- x.axis[level != z]
    x.axis <- x.axis[x.axis[, .I[1], by = group]$V1]   # select the first one.

    # Rotation...
    x.axis[, rotate := FALSE]
    x.axis[dx > 0, rotate := (sign(level - z) == sign(m))]
    x.axis[dx < 0, rotate := (sign(level - z) != sign(m))]

    # x axis
    y.axis <- segments[y.axis == TRUE]
    y.axis[, y.axis := NULL]
    y.axis[, x.d := .second(x.data, x), by = .(x, group)]
    y.axis[, m := x - x.d]

    y.axis <- data[, .(x.d = x, y, z)][y.axis, on = c("x.d", "y")]
    y.axis <- y.axis[level != z]
    y.axis <- y.axis[y.axis[, .I[1], by = group]$V1]

    y.axis[, rotate := FALSE]
    y.axis[dy > 0, rotate := (sign(level - z) != sign(m))]
    y.axis[dy < 0, rotate := (sign(level - z) == sign(m))]

    rot.groups <- c(as.numeric(y.axis[rotate == TRUE]$group),
                    as.numeric(x.axis[rotate == TRUE]$group))

    # rot.groups <- c(as.character(y.axis$group), as.character(x.axis$group))

    contours[, rotate := as.numeric(group[1]) %in% rot.groups, by = group]
    contours <- contours[contours[, ifelse(rotate == TRUE, .I[.N:1], .I), by = group]$V1]

    # Congratulations, your contours all have the same direction.
    return(contours)
}

.order_contour.m <- memoise::memoise(.order_contour)

.second <- function(x, target) {
    tmp <- (x - target)
    x[order(abs(tmp))][2]
}

# from ggplot2
isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
  x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z

  raster
}

.contour_lines <- function(data, breaks, complete = FALSE) {
  z <- isoband_z_matrix(data)

  if (is.list(z)) {
    stopf("Contour requires single 'z' at each combination of 'x' and 'y'.",
         call. = FALSE)
  }

  cl <- isoband::isolines(x = sort(unique(data$x)),
                          y = sort(unique(data$y)),
                          z = z,
                          levels = breaks)


  if (length(cl) == 0) {
    warningf("Not possible to generate contour data.", call. = FALSE)
    return(data.frame())
  }

  # Convert list of lists into single data frame

  cont <- data.table::rbindlist(lapply(cl, data.table::as.data.table), idcol = "level")

  cont[, level := as.numeric(level)]
  cont[, piece := as.numeric(factor(level))]
  cont[, group := factor(paste(data$group[1], sprintf("%03d", piece),  sprintf("%03d", id), sep = "-"))]

  cont[, .(level, x, y, piece, group)]

}


setup_breaks <- function(data, breaks, bins, binwidth) {
  # Check is.null(breaks) for backwards compatibility
  if (is.null(breaks)) {
    breaks <- scales::fullseq
  }

  if (is.function(breaks)) {
    # If no parameters set, use pretty bins to calculate binwidth
    if (is.null(bins) && is.null(binwidth)) {
      binwidth <- diff(pretty(range(data$z, na.rm = TRUE), 10))[1]
    }
    # If provided, use bins to calculate binwidth
    if (!is.null(bins)) {
      binwidth <- diff(range(data$z, na.rm = TRUE)) / bins
    }

    breaks <- breaks(range(data$z, na.rm = TRUE), binwidth)
  }

  return(breaks)
}



# .contour_lines_irregular <- function(data, breaks, complete = FALSE) {
#
#     cl <- setDT(contoureR::getContourLines(
#         x = data$x, y = data$y, z = data$z, levels = breaks))
#
#     if (length(cl) == 0) {
#         warningf("Not possible to generate contour data", call. = FALSE)
#         return(data.frame())
#     }
#     setnames(cl, c("z", "Group", "PID"), c("level", "group", "piece"))
#     return(cl)
# }


#' @rdname geom_text_contour
#' @usage NULL
#' @format NULL
#' @export
StatTextContour <- ggplot2::ggproto("StatTextContour", StatContour2,
  required_aes = c("x", "y", "z"),
  default_aes = ggplot2::aes(order = ..level.., label = ..level..)
)

