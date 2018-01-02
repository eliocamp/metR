#' Discretized continuous color guide
#'
#' A version of [ggplot2::guide_colourbar] that displays discretized values and,
#' by default, puts labels in between values.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @param inside logical indicating where to position labels (see examples).
#'
#' @details
#' The default behaviour works fell for geom_contour_fill in which the colors
#' represent the value between contour surfaces.
#'
#' `inside = TRUE`` works better for geom_tile where the color represents
#' the value of the data and is very similar to gg[ggplot2::guide_legend].
#'
#' @examples
#' # In this example the lowest color represent area of the data with values
#' # between 80 and 100.
#' library(ggplot2)
#' binwidth <- 20
#' data(volcano)
#' ggplot(reshape2::melt(volcano), aes(Var1, Var2, z = value)) +
#'     geom_contour_fill(binwidth = binwidth) +
#'     scale_fill_viridis_c(guide = guide_colourstrip(),
#'                          breaks = MakeBreaks(binwidth))
#'
#' # Difference between guide_legend() and guide_colorbar2(inside = T)
#' df <- reshape2::melt(outer(1:4, 1:4), varnames = c("X1", "X2"))
#' g <- ggplot(df, aes(X1, X2)) +
#'         geom_tile(aes(fill = value)) +
#'         theme(legend.position = "bottom")
#'
#' # Tick labels are to the side
#' g + scale_fill_viridis_c(guide = guide_legend())
#' # Tick labels are at the bottom
#' g + scale_fill_viridis_c(guide = guide_colourstrip(inside = TRUE))
#'
#' @return
#' A guide object.
#'
#' @export
#' @importFrom grid is.unit
guide_colourstrip <- function(
    # title
    title = waiver(),
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

    ...) {

    if (!is.null(barwidth) && !is.unit(barwidth)) barwidth <- unit(barwidth, default.unit)
    if (!is.null(barheight) && !is.unit(barheight)) barheight <- unit(barheight, default.unit)

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
        available_aes = c("colour", "color", "fill"), ..., name = "colorbar"),
        class = c("guide", "colorbar2", "colorbar")
    )
}

#' Methods
#'
#' Documentation for functions that CRAN made me document. DO NOT USE.
#'
#' @param guide,scale things you shouldn't touch
#'
#' @examples
#'
#' print(rep("I will NOT use this functions", 100))
#'
#' @importFrom stats setNames
#' @import gtable
#' @export
#' @rdname dont-document
guide_train.colorbar2 <- function(guide, scale) {
    # do nothing if scale are inappropriate
    if (length(intersect(scale$aesthetics, c("color", "colour", "fill"))) == 0) {
        warning("colorbar2 guide needs colour or fill scales.")
        return(NULL)
    }
    if (scale$is_discrete()) {
        warning("colorbar2 guide needs continuous scales.")
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
        if (is.function(scale$breaks)) {
            breaks <- scale$breaks(.limits)
        } else {
            breaks <- scale$get_breaks()
        }

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

    ticks <- as.data.frame(setNames(list(scale$map(breaks)), scale$aesthetics[1]))
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
guide_colorstip <- guide_colourstrip

.inside <- function(x) {
    N <- length(x)
    x1 <- x[-N] + diff(x)[-N]/2
    # x1[N] <- x[N]
    x1
}
