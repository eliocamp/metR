#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_contour_fill
#' @param breaks One of:
#'   - A numeric vector of breaks
#'   - A function that takes the range of the data and binwidth as input
#'   and returns breaks as output
#' @param bins Number of evenly spaced breaks.
#' @param binwidth Distance between breaks.
# #' @param xwrap,ywrap vector of length two used to wrap the circular dimension
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
                          na.rm = FALSE,
                          na.fill = FALSE,
                          # fill.linear = TRUE,
                          # xwrap = NULL,
                          # ywrap = NULL,
                          show.legend = NA,
                          inherit.aes = TRUE) {
    .check_wrap_param(list(...))
    layer(
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
            # fill.linear = fill.linear,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            # xwrap = xwrap,
            # ywrap = ywrap,
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
      # Check is.null(breaks) for backwards compatibility
      if (is.null(params$breaks)) {
          params$breaks <- scales::fullseq
      }

      if (is.function(params$breaks)) {
          # If no parameters set, use pretty bins to calculate binwidth
          if (is.null(params$bins) && is.null(params$binwidth)) {
              params$binwidth <- diff(pretty(range(data$z, na.rm = TRUE), 10))[1]
          }
          # If provided, use bins to calculate binwidth
          if (!is.null(params$bins)) {
              params$binwidth <- diff(range(data$z, na.rm = TRUE)) / params$bins
          }

          params$breaks <- params$breaks(range(data$z, na.rm = TRUE), params$binwidth)
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
              warning("Computation failed in `", ggplot2:::snake_class(self),
                      "()`:\n",
                      e$message, call. = FALSE)
              data.frame()
          })
      })
  },
  compute_group = function(data, scales, bins = NULL, binwidth = NULL,
                           breaks = scales::fullseq, complete = TRUE,
                           na.rm = FALSE, circular = NULL, xwrap = NULL,
                           ywrap = NULL, na.fill = FALSE) {
      setDT(data)

      data <- data[!(is.na(y) | is.na(x)), ]

      if (isFALSE(na.fill)) {
          data <- data[!is.na(z), ]
      }

      nx <- data[, uniqueN(x), by = y]$V1
      ny <- data[, uniqueN(y), by = x]$V1

      complete.grid <- abs(max(nx) - min(nx)) == 0 & abs(max(ny) - min(ny)) == 0

      if (complete.grid == FALSE) {
          if (complete == FALSE) {
              warning("data must be a complete regular grid", call. = FALSE)
              return(data.frame())
          } else {
              data <- setDT(tidyr::complete(data, x, y, fill = list(z = NA)))
          }
      }

      data <- .impute_data.m(data, na.fill)

      if (!is.null(xwrap)) {
          data <- suppressWarnings(WrapCircular(data, "x", xwrap))
      }
      if (!is.null(ywrap)) {
          data <- suppressWarnings(WrapCircular(data, "y", ywrap))
      }
      setDF(data)
      contours <- as.data.table(.contour_lines(data, breaks, complete = complete))

      if (length(contours) == 0) {
          warning("Not possible to generate contour data", call. = FALSE)
          return(data.frame())
      }
      contours <- .order_contour.m(contours, setDT(data))

      return(contours)
  }
)



.order_contour <- function(contours, data) {
    data <- copy(data)
    contours <- copy(contours)
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

    # x axisd
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

    rot.groups <- c(y.axis[rotate == TRUE]$group,
                    x.axis[rotate == TRUE]$group)

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

.contour_lines <- memoise::memoise(function(data, breaks, complete = FALSE) {
    z <- tapply(data$z, data[c("x", "y")], identity)

    if (is.list(z)) {
        stop("Contour requires single `z` at each combination of `x` and `y`.",
             call. = FALSE)
    }

    cl <- grDevices::contourLines(
        x = sort(unique(data$x)), y = sort(unique(data$y)), z = z,
        levels = breaks)

    if (length(cl) == 0) {
        warning("Not possible to generate contour data", call. = FALSE)
        return(data.frame())
  }

  # Convert list of lists into single data frame
  lengths <- vapply(cl, function(x) length(x$x), integer(1))
  levels <- vapply(cl, "[[", "level", FUN.VALUE = double(1))
  xs <- unlist(lapply(cl, "[[", "x"), use.names = FALSE)
  ys <- unlist(lapply(cl, "[[", "y"), use.names = FALSE)
  pieces <- rep(seq_along(cl), lengths)
  # Add leading zeros so that groups can be properly sorted later
  groups <- paste(data$group[1], sprintf("%03d", pieces), sep = "-")

  data.frame(
    level = rep(levels, lengths),
    x = xs,
    y = ys,
    piece = pieces,
    group = groups
  )
})

# .contour_lines_irregular <- function(data, breaks, complete = FALSE) {
#
#     cl <- setDT(contoureR::getContourLines(
#         x = data$x, y = data$y, z = data$z, levels = breaks))
#
#     if (length(cl) == 0) {
#         warning("Not possible to generate contour data", call. = FALSE)
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

