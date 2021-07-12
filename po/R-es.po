#' Discretized continuous colour guide
#'
#' A version of [ggplot2::guide_colourbar] that displays discretized values and,
#' by default, puts labels in between values.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @param inside logical indicating where to position labels (see examples).
#'
#' @details
#' The default behaviour works fell for geom_contour_fill in which the colours
#' represent the value between contour surfaces.
#'
#' `inside = TRUE`` works better for geom_tile where the colour represents
#' the value of the data and is very similar to [ggplot2::guide_legend].
#'
#' @examples
#' # In this example the lowest colour represents an area of the data with values
#' # between 80 and 100.
#' library(ggplot2)
#' binwidth <- 20
#' data(volcano)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour_fill(binwidth = binwidth) +
#'     scale_fill_continuous(guide = guide_colourstrip(),
#'                          breaks = MakeBreaks(binwidth))
#'
#' # Difference between guide_legend() and guide_colorbar2(inside = T)
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#' g <- ggplot(df, aes(X1, X2)) +
#'         geom_tile(aes(fill = value)) +
#'         theme(legend.position = "bottom")
#'
#' # Tick labels are to the side
#' g + scale_fill_continuous(guide = guide_legend())
#' # Tick labels are at the bottom
#' g + scale_fill_continuous(guide = guide_colourstrip(inside = TRUE))
#'
#' @return
#' A guide object.
#' @family ggplot2 helpers
#' @export
guide_colourstrip <- function(
    # title
    title = ggplot2::waiver(),
    title.position = NULL,
    title.theme = NULL,
    title.hjust = NULL,
    title.vjust = NULL,

    # label
    label = TRUE,
    label.position = NULL,
    label.theme = NULL,
    label.hjust = NULL,
    label.vjust = NULL,

    # bar
    barwidth = NULL,
    barheight = NULL,

    # ticks
    ticks = FALSE,
    draw.ulim= TRUE,
    draw.llim = TRUE,
    inside = FALSE,

    # general
    direction = NULL,
    default.unit = "line",
    reverse = FALSE,
    order = 0,

    available_aes = c("colour", "color", "fill"),
    ...) {

    if (!is.null(barwidth) && !grid::is.unit(barwidth)) barwidth <- grid::unit(barwidth, default.unit)
    if (!is.null(barheight) && !grid::is.unit(barheight)) barheight <- grid::unit(barheight, default.unit)

    structure(list(
        # title
        title = title,
        title.position = title.position,
        title.theme = title.theme,
        title.hjust = title.hjust,
        title.vjust = title.vjust,

        # label
        label = label,
        label.position = label.position,
        label.theme = label.theme,
        label.hjust = label.hjust,
        label.vjust = label.vjust,

        # bar
        barwidth = barwidth,
        barheight = barheight,
        raster = FALSE,

        # ticks
        ticks = ticks,
        draw.ulim = draw.ulim,
        draw.llim = draw.llim,
        inside = inside,

        # general
        direction = direction,
        default.unit = default.unit,
        reverse = reverse,
        order = order,

        # parameter
        available_aes = available_aes, ..., name = "colorbar"),
        class = c("guide", "colorstrip", "colorbar")
    )
}

#' @usage NULL
#' @format NULL
#' @export
#' @rdname guide_colourstrip
#' @keywords internal
#' @importFrom ggplot2 guide_train
guide_train.colorstrip <- function(guide, scale, aesthetic = NULL) {
    # do nothing if scale are inappropriate
    if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
        warning("colorstrip guide needs colour or fill scales.")
        return(NULL)
    }
    if (scale$is_discrete()) {
        warning("colorstrip guide needs continuous scales.")
        return(NULL)
    }

    # bar specification (number of divs etc)
    .limits <- scale$get_limits()

    # create data frame for tick display

    if (guide$inside) {
        breaks <- scale$get_breaks()
        breaks <- breaks[!is.na(breaks)]
        guide$nbin <- length(breaks)
        .bar <- breaks
    } else {
        breaks <- .get_breaks(scale)
        .bar <- .inside(breaks[!is.na(breaks)])
        guide$nbin <- length(.bar)
    }
    if (length(breaks) == 0 || all(is.na(breaks)))
        return()

    if (length(.bar) == 0) {
        .bar = unique(.limits)
    }

    guide$bar <- data.frame(colour = scale$map(.bar), value = .bar,
                            stringsAsFactors = FALSE)

    ticks <- as.data.frame(stats::setNames(list(scale$map(breaks)), scale$aesthetics[1]))
    ticks$.value <- breaks
    ticks$.label <- scale$get_labels(breaks)

    guide$key <- ticks

    if (guide$reverse) {
        guide$key <- guide$key[nrow(guide$key):1, ]
        guide$bar <- guide$bar[nrow(guide$bar):1, ]
    }

    guide$hash <- with(guide, digest::digest(list(title, key$.label, bar, name)))
    guide
}


#' @export
#' @rdname guide_colourstrip
guide_colorstrip <- guide_colourstrip

.inside <- function(x) {
    N <- length(x)
    x1 <- x[-N] + diff(x)[-N]/2
    # x1[N] <- x[N]
    x1
}



.get_breaks = function(scale, limits = scale$get_limits()) {
    if (scale$is_empty()) return(numeric())

    # Limits in transformed space need to be converted back to data space
    limits <- scale$trans$inverse(limits)

    if (is.null(scale$breaks)) {
        return(NULL)
    } else if (identical(scale$breaks, NA)) {
        stop("Invalid breaks specification. Use NULL, not NA")
    } else if (scales::zero_range(as.numeric(limits))) {
        breaks <- limits[1]
    } else if (is.waive(scale$breaks)) {
        breaks <- scale$trans$breaks(limits)
    } else if (is.function(scale$breaks)) {
        breaks <- scale$breaks(limits)
    } else {
        breaks <- scale$breaks
    }

    breaks <- scale$trans$transform(breaks)
    breaks
}


#' @usage NULL
#' @format NULL
#' @export
#' @rdname guide_colourstrip
#' @keywords internal
#' @importFrom ggplot2 guide_gengrob
guide_gengrob.colorstrip <- function(guide, theme) {
    # settings of location and size
    switch(guide$direction,
           "horizontal" = {
               label.position <- guide$label.position %||% "bottom"
               if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")

               barwidth <- grid::convertWidth(guide$barwidth %||% (theme$legend.key.width * 5), "mm")
               barheight <- grid::convertHeight(guide$barheight %||% theme$legend.key.height, "mm")
           },
           "vertical" = {
               label.position <- guide$label.position %||% "right"
               if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")

               barwidth <- grid::convertWidth(guide$barwidth %||% theme$legend.key.width, "mm")
               barheight <- grid::convertHeight(guide$barheight %||% (theme$legend.key.height * 5), "mm")
           })

    barwidth.c <- c(barwidth)
    barheight.c <- c(barheight)
    barlength.c <- switch(guide$direction, "horizontal" = barwidth.c, "vertical" = barheight.c)
    nbreak <- nrow(guide$key)

    tic_pos.c <- scales::rescale(guide$key$.value, c(0.5, guide$nbin - 0.5), guide$bar$value[c(1, nrow(guide$bar))]) * barlength.c / guide$nbin
    grob.bar <-
            switch(guide$direction,
                   horizontal = {
                       if (guide$inside) {
                           bx <- .inside(tic_pos.c)
                           bx <- c(2*tic_pos.c[1] - bx[1], bx)
                           bw <- c(diff(bx), 2*(tic_pos.c[length(tic_pos.c)] - bx[length(bx)]))
                       } else {
                           bx <- tic_pos.c[-length(tic_pos.c)]
                           bw <- diff(tic_pos.c)
                       }

                       grid::rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight.c, default.units = "mm",
                                      gp = grid::gpar(col = NA, fill = guide$bar$colour))
                   },
                   vertical = {
                       if (guide$inside) {
                           by <- .inside(tic_pos.c)
                           by <- c(2*tic_pos.c[1] - by[1], by)
                           bh <- c(diff(by), 2*(tic_pos.c[length(tic_pos.c)] - by[length(by)]))
                       } else {
                           by <- tic_pos.c[-length(tic_pos.c)]
                           bh <- diff(tic_pos.c)
                       }

                       grid::rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth.c, height = bh, default.units = "mm",
                                      gp = grid::gpar(col = NA, fill = guide$bar$colour))
                   })


    # tick and label position

    label_pos <- grid::unit(tic_pos.c, "mm")
    if (!guide$draw.ulim) tic_pos.c <- tic_pos.c[-1]
    if (!guide$draw.llim) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

    # title
    grob.title <- ggname("guide.title",
                         ggplot2::element_grob(
                             guide$title.theme %||% ggplot2::calc_element("legend.title", theme),
                             label = guide$title,
                             hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
                             vjust = guide$title.vjust %||% 0.5
                             )
    )


    title_width <- grid::convertWidth(grid::grobWidth(grob.title), "mm")
    title_width.c <- c(title_width)
    title_height <- grid::convertHeight(grid::grobHeight(grob.title), "mm")
    title_height.c <- c(title_height)

    # gap between keys etc
    hgap <- width_cm(theme$legend.spacing.x  %||% grid::unit(0.3, "line"))
    vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * grid::unit(title_height, "cm")))

    # label
    label.theme <- guide$label.theme %||% ggplot2::calc_element("legend.text", theme)
    grob.label <- {
        if (!guide$label)
            ggplot2::zeroGrob()
        else {
            hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
                if (any(is.expression(guide$key$.label))) 1 else switch(guide$direction, horizontal = 0.5, vertical = 0)
            vjust <- y <- guide$label.vjust %||% 0.5
            switch(guide$direction, horizontal = {x <- label_pos; y <- vjust}, "vertical" = {x <- hjust; y <- label_pos})

            label <- guide$key$.label

            # If any of the labels are quoted language objects, convert them
            # to expressions. Labels from formatter functions can return these
            if (any(vapply(label, is.call, logical(1)))) {
                label <- lapply(label, function(l) {
                    if (is.call(l)) substitute(expression(x), list(x = l))
                    else l
                })
                label <- do.call(c, label)
            }
            g <- ggplot2::element_grob(element = label.theme, label = label,
                                       x = x, y = y, hjust = hjust, vjust = vjust)
            ggname("guide.label", g)
        }
    }

    label_width <- grid::convertWidth(grid::grobWidth(grob.label), "mm")
    label_width.c <- c(label_width)
    label_height <- grid::convertHeight(grid::grobHeight(grob.label), "mm")
    label_height.c <- c(label_height)

    # ticks
    grob.ticks <-
        if (!guide$ticks) ggplot2::zeroGrob()
    else {
        switch(guide$direction,
               "horizontal" = {
                   x0 = rep(tic_pos.c, 2)
                   y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak))
                   x1 = rep(tic_pos.c, 2)
                   y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak))
               },
               "vertical" = {
                   x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak))
                   y0 = rep(tic_pos.c, 2)
                   x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak))
                   y1 = rep(tic_pos.c, 2)
               })
        grid::segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                     default.units = "mm", gp = grid::gpar(col = "white", lwd = 0.5, lineend = "butt"))
    }

    # layout of bar and label
    switch(guide$direction,
           "horizontal" = {
               switch(label.position,
                      "top" = {
                          bl_widths <- barwidth.c
                          bl_heights <- c(label_height.c, vgap, barheight.c)
                          vps <- list(bar.row = 3, bar.col = 1,
                                      label.row = 1, label.col = 1)
                      },
                      "bottom" = {
                          bl_widths <- barwidth.c
                          bl_heights <- c(barheight.c, vgap, label_height.c)
                          vps <- list(bar.row = 1, bar.col = 1,
                                      label.row = 3, label.col = 1)
                      })
           },
           "vertical" = {
               switch(label.position,
                      "left" = {
                          bl_widths <- c(label_width.c, vgap, barwidth.c)
                          bl_heights <- barheight.c
                          vps <- list(bar.row = 1, bar.col = 3,
                                      label.row = 1, label.col = 1)
                      },
                      "right" = {
                          bl_widths <- c(barwidth.c, vgap, label_width.c)
                          bl_heights <- barheight.c
                          vps <- list(bar.row = 1, bar.col = 1,
                                      label.row = 1, label.col = 3)
                      })
           })

    # layout of title and bar+label
    switch(guide$title.position,
           "top" = {
               widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
               heights <- c(title_height.c, vgap, bl_heights)
               vps <- with(vps,
                           list(bar.row = bar.row + 2, bar.col = bar.col,
                                label.row = label.row + 2, label.col = label.col,
                                title.row = 1, title.col = seq_along(widths)))
           },
           "bottom" = {
               widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
               heights <- c(bl_heights, vgap, title_height.c)
               vps <- with(vps,
                           list(bar.row = bar.row, bar.col = bar.col,
                                label.row = label.row, label.col = label.col,
                                title.row = length(heights), title.col = seq_along(widths)))
           },
           "left" = {
               widths <- c(title_width.c, hgap, bl_widths)
               heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
               vps <- with(vps,
                           list(bar.row = bar.row, bar.col = bar.col + 2,
                                label.row = label.row, label.col = label.col + 2,
                                title.row = seq_along(heights), title.col = 1))
           },
           "right" = {
               widths <- c(bl_widths, hgap, title_width.c)
               heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
               vps <- with(vps,
                           list(bar.row = bar.row, bar.col = bar.col,
                                label.row = label.row, label.col = label.col,
                                title.row = seq_along(heights), title.col = length(widths)))
           })

    # background
    grob.background <- element_render(theme, "legend.background")

    # padding
    padding <- grid::convertUnit(theme$legend.margin %||% ggplot2::margin(), "mm")
    widths <- c(padding[4], widths, padding[2])
    heights <- c(padding[1], heights, padding[3])

    gt <- gtable::gtable(widths = grid::unit(widths, "mm"), heights = grid::unit(heights, "mm"))
    gt <- gtable::gtable_add_grob(gt, grob.background, name = "background", clip = "off",
                          t = 1, r = -1, b = -1, l = 1)
    gt <- gtable::gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
                          t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                          b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
    gt <- gtable::gtable_add_grob(gt, grob.label, name = "label", clip = "off",
                          t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
                          b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
    gt <- gtable::gtable_add_grob(gt, grob.title, name = "title", clip = "off",
                          t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
                          b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
    gt <- gtable::gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
                          t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                          b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))

    gt
}
