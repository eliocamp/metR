#' Reference arrow for magnitude scales
#'
#' Draws a reference arrow. Highly experimental.
#'
#' @inheritParams ggplot2::guide_legend
#'
#' @seealso scale_vector
#' @export
guide_vector <- function(# title
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

    # key
    keywidth = NULL,
    keyheight = NULL,

    # general
    direction = NULL,
    default.unit = "cm",
    override.aes = list(),
    nrow = NULL,
    ncol = NULL,
    byrow = FALSE,
    reverse = FALSE,
    order = 0,
    ...) {

    if (!is.null(keywidth) && !grid::is.unit(keywidth)) {
        keywidth <- grid::unit(keywidth, default.unit)
    }
    if (!is.null(keyheight) && !grid::is.unit(keyheight)) {
        keyheight <- grid::unit(keyheight, default.unit)
    }

    structure(
        list(
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

            # size of key
            keywidth = keywidth,
            keyheight = keyheight,
            default.unit = default.unit,

            # general
            direction = direction,
            override.aes = rename_aes(override.aes),
            nrow = nrow,
            ncol = ncol,
            byrow = byrow,
            reverse = reverse,
            order = order,

            # parameter
            available_aes = c("any"),
            ...,
            name = "vector"
        ),
        class = c("guide", "vector", "legend")
    )
}


#' @importFrom ggplot2 guide_train guide_gengrob
#' @export
guide_train.vector <- function(guide, scale, aesthetic = NULL) {
    limits <- scale$get_limits()
    limits[1] <- 0
    breaks <- .get_breaks(scale, limits)
    if (length(breaks) == 0 || all(is.na(breaks))) {
        return()
    }

    key <- as.data.frame(
        stats::setNames(list(scale$map(breaks)), scale$aesthetics[1]),
        stringsAsFactors = FALSE
    )
    key$.label <- scale$get_labels(breaks)

    if (guide$reverse) key <- key[nrow(key):1, ]

    guide$keywidth <- grid::unit(key[1, 1], guide$default.unit)

    guide$key <- key
    guide$hash <- with(
        guide,
        digest::digest(list(title, key$.label, direction, name))
    )

    guide
}

#' @export
#' @importFrom ggplot2 guide_geom
guide_geom.vector <- function(guide, layers, ...) {
    if (!inherits(ggplot2::guide_none(), "Guide")) {
        return(NextMethod())
    }
    legend <- ggplot2::guide_legend()
    legend$get_layer_key(guide, layers, rep(list(NULL), length(layers)))
}

#' @export
#' @importFrom ggplot2 guide_gengrob
guide_gengrob.vector <- function(guide, theme) {
    if (!inherits(ggplot2::guide_none(), "Guide")) {
        return(NextMethod())
    }
    position  <- theme$legend.position %||% "right"
    direction <- theme$legend.direction %||% switch(
        position, top = , bottom = "horizontal", "vertical"
    )
    theme$legend.key.width <- guide$keywidth
    legend <- ggplot2::guide_legend()
    legend$draw(theme, position = position, direction = direction, params = guide)
}


globalVariables(c("C", "R", "key.row", "key.col", "label.row", "label.col"))

