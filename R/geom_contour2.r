#' 2d contours of a 3d surface
#'
#' Similar to [ggplot2::geom_contour] but it can label contour lines,
#' accepts accepts a function as the `breaks` argument and and computes
#' breaks globally instead of per panel.
#'
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_contour_fill
#' @param skip number of contours to skip for labelling
#' (e.g. `skip = 1` will skip 1 contour line between labels).
#' @param label.placer a label placer function. See [label_placer_flattest()].
#' @param margin the margin around labels around which contour lines
#' are clipped to avoid overlapping.
#'
#'
#' @section Aesthetics:
#' \code{geom_contour2} understands the following aesthetics (required aesthetics are in bold):
#'
#' Aesthetics related to contour lines:
#' \itemize{
#' \item \strong{x}
#' \item \strong{y}
#' \item \strong{z}
#'  \item \code{alpha}
#'  \item \code{colour}
#'  \item \code{group}
#'  \item \code{linetype}
#'  \item \code{size}
#'  \item \code{weight}
#'  }
#'
#'  Aesthetics related to labels:
#'  \itemize{
#'  \item \code{label}
#'  \item \code{label_colour}
#'  \item \code{label_alpha}
#'  \item \code{label_size}
#'  \item \code{family}
#'  \item \code{fontface}
#'}
#'
#' @examplesIf requireNamespace("reshape2")
#'
#' \dontshow{data.table::setDTthreads(1)}
#'
#' library(ggplot2)
#'
#' # Breaks can be a function.
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, color = after_stat(level)),
#'                   breaks = AnchorBreaks(130, binwidth = 10))
#'
#' # Add labels by supplying the label aes.
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, label = after_stat(level)))
#'
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, label = after_stat(level)),
#'                   skip = 0)
#'
#' # Use label.placer to control where contours are labelled.
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, label = after_stat(level)),
#'                       label.placer = label_placer_n(n = 2))
#'
#' # Use the rot_adjuster argument of the placer function to
#' # control the angle. For example, to fix it to some angle:
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2)) +
#'     geom_contour2(aes(z = value, label = after_stat(level)),
#'                   skip = 0,
#'                   label.placer = label_placer_flattest(rot_adjuster = 0))
#'
#' @family ggplot2 helpers
#' @export
geom_contour2 <- function(mapping = NULL, data = NULL,
                          stat = "contour2", position = "identity",
                          ...,
                          lineend = "butt",
                          linejoin = "round",
                          linemitre = 1,
                          breaks = MakeBreaks(),
                          bins = NULL,
                          binwidth = NULL,
                          global.breaks = TRUE,
                          na.rm = FALSE,
                          na.fill = FALSE,
                          skip = 1,
                          margin = grid::unit(c(1, 1, 1, 1), "pt"),
                          label.placer = label_placer_flattest(),
                          show.legend = NA,
                          inherit.aes = TRUE) {
    .check_wrap_param(list(...))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomContour2,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre,
            breaks = breaks,
            bins = bins,
            binwidth = binwidth,
            global.breaks = global.breaks,
            na.rm = na.rm,
            na.fill = na.fill,
            skip = skip,
            margin = margin,
            label.placer = label.placer,
            ...
        )
    )
}




#' @rdname geom_contour2
#' @usage NULL
#' @format NULL
#' @export
GeomContour2 <- ggplot2::ggproto("GeomContour2", ggplot2::GeomContour,
  default_aes = ggplot2::aes(weight = 1, colour = "black", linewidth = 0.5, linetype = 1,
                             alpha = NA, label = NULL, label_colour = NULL,
                             label_alpha = NULL, label_size = 3.88,
                             family = "", fontface = 1),
  rename_size = TRUE,
  non_missing_aes = "size",
  draw_panel = function(data, panel_params, coord, arrow = NULL, lineend = "butt",
                        linejoin = "round", linemitre = 10, na.rm = FALSE,
                        skip = 1, margin = grid::unit(c(1, 1, 1, 1), "pt"),
                        label.placer = label_placer_flattest()) {
      data <- data[order(data$group), , drop = FALSE]
      munched <- ggplot2::coord_munch(coord, data, panel_params)
      rows <- stats::ave(seq_len(nrow(munched)), munched$group,
                         FUN = length)
      munched <- munched[rows >= 2, ]

      lines <- .contours_to_isolines(munched)
      colour <- .extract_attr(munched, "colour")
      size <- .extract_attr(munched, "size")
      linewidth <- .extract_attr(munched, "linewidth")
      linetype <- .extract_attr(munched, "linetype")
      alpha <- .extract_attr(munched, "alpha")
      alpha[is.na(alpha)] <- 1
      label <- .extract_attr(munched, "label")

      label_colour <- .extract_attr(munched, "label_colour")
      label_alpha <- .extract_attr(munched, "label_alpha")
      label_size <- .extract_attr(munched, "label_size")
      family <- .extract_attr(munched, "family")
      fontface <- .extract_attr(munched, "fontface")

      skip <- skip + 1
      if (is.null(label)) {
          breaks <- ""
      } else {
          breaks <- JumpBy(names(lines), skip)
          label <- JumpBy(label, skip)
          label_size <- JumpBy(label_size, skip)
          family <- JumpBy(family, skip)
          fontface <- JumpBy(fontface, skip)

          if (!is.null(label_colour)) {
            label_colour <- JumpBy(label_colour, skip)
          }

          if (!is.null(label_alpha)) {
            label_alpha <- JumpBy(label_alpha, skip)
          }
      }

      isoband::isolines_grob(
        lines,
        label_placer = label.placer,
        breaks =  breaks,
        labels = label,
        margin = margin,
        label_col = label_colour,
        label_alpha = label_alpha,
        gp = grid::gpar(
          fontfamily = family,
          fontface = fontface,
          fontsize = label_size*.pt,
          lwd = (linewidth %||% size) * .pt,
          col = colour,
          lty = linetype,
          alpha = alpha
        )
      )

  })

.extract_attr <- function(contours, column) {
    levels <- unique(contours$level)

    unlist(lapply(levels, function(l) {
        data_subset <- contours[contours$level == l, ]
        val <- unique(data_subset[[column]])
        if (length(val) > 1) {
            stop("more than one unique value for ", column)
        }
        val
    }))

}
.contours_to_isolines <- function(contours) {
    levels <- unique(contours$level)


    lines <- lapply(levels, function(l) {
        data_subset <- contours[contours$level == l, ]

        list(x = data_subset$x,
             y = data_subset$y,
             id = as.integer(data_subset$group))
    })

    class(lines) <- c("isolines", "iso")
    names(lines) <- levels

    lines
}



.check_wrap_param <- function(params) {
    if (!is.null(params$xwrap) | !is.null(params$ywrap)) {
        warningf("'xwrap' and 'ywrap' will be deprecated. Use ggperiodic::periodic insead.")
    }
}
