#' Reference arrow for magnitude scales
#'
#' Draws a reference arrow. Highly experimental.
#'
#' @inheritParams ggplot2::guide_legend
#'
#' @seealso scale_vector
#' @export
guide_vector <- function(
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
        class = c("guide", "vector")
    )
}

#' @export
guide_train.vector <- function(guide, scale, output = NULL) {
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
guide_merge.vector <- function(guide, new_guide) {
    guide$key <- merge(guide$key, new_guide$key, sort = FALSE)
    guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
    if (any(duplicated(names(guide$override.aes)))) {
        warning("Duplicated override.aes is ignored.")
    }
    guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
    guide
}

#' @export
guide_geom.vector <- function(guide, layers, default_mapping) {
    # arrange common data for vertical and horizontal guide
    guide$geoms <- plyr::llply(layers, function(layer) {
        matched <- matched_aes(layer, guide, default_mapping)

        if (length(matched) > 0) {
            # This layer contributes to the legend

            # check if this layer should be included, different behaviour depending on
            # if show.legend is a logical or a named logical vector
            if (!is.null(names(layer$show.legend))) {
                layer$show.legend <- rename_aes(layer$show.legend)
                include <- is.na(layer$show.legend[matched]) ||
                    layer$show.legend[matched]
            } else {
                include <- is.na(layer$show.legend) || layer$show.legend
            }

            if (include) {
                # Default is to include it

                # Filter out set aesthetics that can't be applied to the legend
                n <- vapply(layer$aes_params, length, integer(1))
                params <- layer$aes_params[n == 1]

                data <- layer$geom$use_defaults(guide$key[matched], params)
            } else {
                return(NULL)
            }
        } else {
            # This layer does not contribute to the legend
            if (is.na(layer$show.legend) || !layer$show.legend) {
                # Default is to exclude it
                return(NULL)
            } else {
                data <- layer$geom$use_defaults(NULL, layer$aes_params)[rep(1, nrow(guide$key)), ]
            }
        }

        # override.aes in guide_legend manually changes the geom
        data <- utils::modifyList(data, guide$override.aes)

        list(
            draw_key = layer$geom$draw_key,
            data = data,
            params = c(layer$geom_params, layer$stat_params)
        )
    })

    # remove null geom
    guide$geoms <- plyr::compact(guide$geoms)

    # Finally, remove this guide if no layer is drawn
    if (length(guide$geoms) == 0) guide <- NULL
    guide
}

#' @export
guide_gengrob.vector <- function(guide, theme) {

    # default setting
    label.position <- guide$label.position %||% "right"
    if (!label.position %in% c("top", "bottom", "left", "right"))
        stop("label position \"", label.position, "\" is invalid")

    nbreak <- nrow(guide$key)

    grob.title <- ggname("guide.title",
                         element_grob(
                             guide$title.theme %||% calc_element("legend.title", theme),
                             label = guide$title,
                             hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
                             vjust = guide$title.vjust %||% 0.5,
                             margin_x = FALSE,
                             margin_y = FALSE
                         )
    )

    title_width <- width_cm(grob.title)
    title_height <- height_cm(grob.title)

    # gap between keys etc
    hgap <- width_cm(theme$legend.spacing.x  %||% unit(0.3, "line"))
    vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_height, "cm")))

    # Labels
    if (!guide$label || is.null(guide$key$.label)) {
        grob.labels <- rep(list(zeroGrob()), nrow(guide$key))
    } else {
        label.theme <- guide$label.theme %||% calc_element("legend.text", theme)

        hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
            if (any(is.expression(guide$key$.label))) 1 else 0
        vjust <- y <- guide$label.vjust %||% 0.5

        grob.labels <- lapply(guide$key$.label, function(label, ...) {
            g <- element_grob(
                element = label.theme,
                label = label,
                x = x,
                y = y,
                hjust = hjust,
                vjust = vjust,
                margin_x = FALSE,
                margin_y = FALSE
            )
            ggname("guide.label", g)
        })
    }

    label_widths <- width_cm(grob.labels)
    label_heights <- height_cm(grob.labels)

    # Keys
    key_width <- width_cm(
        guide$keywidth %||% theme$legend.key.width %||% theme$legend.key.size
    )
    key_height <- height_cm(
        guide$keyheight %||% theme$legend.key.height %||% theme$legend.key.size
    )

    key_size_mat <- do.call("cbind", lapply(guide$geoms, function(g) g$data$size / 10))
    if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
        key_size_mat <- matrix(0, ncol = 1, nrow = nbreak)
    }
    key_sizes <- apply(key_size_mat, 1, max)

    if (!is.null(guide$nrow) && !is.null(guide$ncol) &&
        guide$nrow * guide$ncol < nbreak) {
        stop(
            "`nrow` * `ncol` needs to be larger than the number of breaks",
            call. = FALSE
        )
    }

    # If neither nrow/ncol specified, guess with "reasonable" values
    if (is.null(guide$nrow) && is.null(guide$ncol)) {
        if (guide$direction == "horizontal") {
            guide$nrow <- ceiling(nbreak / 5)
        } else {
            guide$ncol <- ceiling(nbreak / 20)
        }
    }
    legend.nrow <- guide$nrow %||% ceiling(nbreak / guide$ncol)
    legend.ncol <- guide$ncol %||% ceiling(nbreak / guide$nrow)

    key_sizes <- matrix(
        c(key_sizes, rep(0, legend.nrow * legend.ncol - nbreak)),
        legend.nrow,
        legend.ncol,
        byrow = guide$byrow
    )

    key_widths <- pmax(key_width, apply(key_sizes, 2, max))
    key_heights <- pmax(key_height, apply(key_sizes, 1, max))

    label_widths <- apply(
        matrix(
            c(label_widths, rep(0, legend.nrow * legend.ncol - nbreak)),
            legend.nrow,
            legend.ncol,
            byrow = guide$byrow
        ),
        2,
        max
    )
    label_heights <- apply(
        matrix(
            c(label_heights, rep(0, legend.nrow * legend.ncol - nbreak)),
            legend.nrow,
            legend.ncol,
            byrow = guide$byrow
        ),
        1,
        max
    )

    if (guide$byrow) {
        vps <- data.frame(
            R = ceiling(seq(nbreak) / legend.ncol),
            C = (seq(nbreak) - 1) %% legend.ncol + 1
        )
    } else {
        vps <- as.data.frame(arrayInd(seq(nbreak), dim(key_sizes)))
        names(vps) <- c("R", "C")
    }

    # layout of key-label depends on the direction of the guide
    if (guide$byrow == TRUE) {
        switch(
            label.position,
            "top" = {
                kl_widths <- pmax(label_widths, key_widths)
                kl_heights <- utils::head(
                    interleave(label_heights, vgap / 2, key_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 1,
                    key.col = C,
                    label.row = R * 4 - 3,
                    label.col = C
                )
            },
            "bottom" = {
                kl_widths <- pmax(label_widths, key_widths)
                kl_heights <- utils::head(
                    interleave(key_heights, vgap / 2, label_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 3,
                    key.col = C,
                    label.row = R * 4 - 1,
                    label.col = C
                )
            },
            "left" = {
                kl_widths <- utils::head(
                    interleave(label_widths, hgap / 2, key_widths, hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(pmax(label_heights, key_heights), vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 2 - 1,
                    key.col = C * 4 - 1,
                    label.row = R * 2 - 1,
                    label.col = C * 4 - 3
                )
            },
            "right" = {
                kl_widths <- utils::head(
                    interleave(key_widths, hgap / 2, label_widths, hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(pmax(label_heights, key_heights), vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 2 - 1,
                    key.col = C * 4 - 3,
                    label.row = R * 2 - 1,
                    label.col = C * 4 - 1
                )
            })
    } else {
        switch(
            label.position,
            "top" = {
                kl_widths <- utils::head(
                    interleave(pmax(label_widths, key_widths), hgap/2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(label_heights, vgap / 2, key_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 1,
                    key.col = C * 2 - 1,
                    label.row = R * 4 - 3,
                    label.col = C * 2 - 1
                )
            },
            "bottom" = {
                kl_widths <- utils::head(
                    interleave(pmax(label_widths, key_widths), hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(key_heights, vgap / 2, label_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 3,
                    key.col = C * 2 - 1,
                    label.row = R * 4 - 1,
                    label.col = C * 2 - 1
                )
            },
            "left" = {
                kl_widths <- utils::head(
                    interleave(label_widths, hgap / 2, key_widths, hgap / 2),
                    -1
                )
                kl_heights <- pmax(key_heights, label_heights)
                vps <- transform(
                    vps,
                    key.row = R,
                    key.col = C * 4 - 1,
                    label.row = R,
                    label.col = C * 4 - 3
                )
            },
            "right" = {
                kl_widths <- utils::head(
                    interleave(key_widths, hgap / 2, label_widths, hgap / 2),
                    -1
                )
                kl_heights <- pmax(key_heights, label_heights)
                vps <- transform(
                    vps,
                    key.row = R,
                    key.col = C * 4 - 3,
                    label.row = R,
                    label.col = C * 4 - 1
                )
            })
    }

    # layout the title over key-label
    switch(guide$title.position,
           "top" = {
               widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
               heights <- c(title_height, vgap, kl_heights)
               vps <- transform(
                   vps,
                   key.row = key.row + 2,
                   key.col = key.col,
                   label.row = label.row + 2,
                   label.col = label.col
               )
               vps.title.row = 1; vps.title.col = seq_along(widths)
           },
           "bottom" = {
               widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
               heights <- c(kl_heights, vgap, title_height)
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col,
                   label.row = label.row,
                   label.col = label.col
               )
               vps.title.row = length(heights); vps.title.col = seq_along(widths)
           },
           "left" = {
               widths <- c(title_width, hgap, kl_widths)
               heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col + 2,
                   label.row = label.row,
                   label.col = label.col + 2
               )
               vps.title.row = seq_along(heights); vps.title.col = 1
           },
           "right" = {
               widths <- c(kl_widths, hgap, title_width)
               heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col,
                   label.row = label.row,
                   label.col = label.col
               )
               vps.title.row = seq_along(heights); vps.title.col = length(widths)
           })

    # grob for key
    key_size <- c(key_width, key_height) * 10

    draw_key <- function(i) {
        bg <- element_render(theme, "legend.key")
        keys <- lapply(guide$geoms, function(g) {
            g$draw_key(g$data[i, ], g$params, key_size)
        })
        c(list(bg), keys)
    }
    grob.keys <- unlist(lapply(seq_len(nbreak), draw_key), recursive = FALSE)

    # background
    grob.background <- element_render(theme, "legend.background")

    ngeom <- length(guide$geoms) + 1
    kcols <- rep(vps$key.col, each = ngeom)
    krows <- rep(vps$key.row, each = ngeom)

    # padding
    padding <- grid::convertUnit(theme$legend.margin %||% ggplot2::margin(), "cm")
    widths <- c(padding[4], widths, padding[2])
    heights <- c(padding[1], heights, padding[3])

    # Create the gtable for the legend
    gt <- gtable::gtable(widths = grid::unit(widths, "cm"), heights = grid::unit(heights, "cm"))
    gt <- gtable::gtable_add_grob(
        gt,
        grob.background,
        name = "background",
        clip = "off",
        t = 1,
        r = -1,
        b = -1,
        l = 1
    )
    gt <- gtable_add_grob(
        gt,
        grob.title,
        name = "title",
        clip = "off",
        t = 1 + min(vps.title.row),
        r = 1 + max(vps.title.col),
        b = 1 + max(vps.title.row),
        l = 1 + min(vps.title.col)
    )
    gt <- gtable::gtable_add_grob(
        gt,
        grob.keys,
        name = paste("key", krows, kcols, c("bg", seq(ngeom - 1)), sep = "-"),
        clip = "off",
        t = 1 + krows,
        r = 1 + kcols,
        b = 1 + krows,
        l = 1 + kcols
    )
    gt <- gtable::gtable_add_grob(
        gt,
        grob.labels,
        name = paste("label", vps$label.row, vps$label.col, sep = "-"),
        clip = "off",
        t = 1 + vps$label.row,
        r = 1 + vps$label.col,
        b = 1 + vps$label.row,
        l = 1 + vps$label.col
    )
    gt
}

#' @export
guide_vector <- function(# title
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
        class = c("guide", "vector")
    )
}

#' @export
guide_train.vector <- function(guide, scale, output = NULL) {
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
guide_merge.vector <- function(guide, new_guide) {
    guide$key <- merge(guide$key, new_guide$key, sort = FALSE)
    guide$override.aes <- c(guide$override.aes, new_guide$override.aes)
    if (any(duplicated(names(guide$override.aes)))) {
        warning("Duplicated override.aes is ignored.")
    }
    guide$override.aes <- guide$override.aes[!duplicated(names(guide$override.aes))]
    guide
}

#' @export
guide_geom.vector <- function(guide, layers, default_mapping) {
    # arrange common data for vertical and horizontal guide
    guide$geoms <- plyr::llply(layers, function(layer) {
        matched <- matched_aes(layer, guide, default_mapping)

        if (length(matched) > 0) {
            # This layer contributes to the legend

            # check if this layer should be included, different behaviour depending on
            # if show.legend is a logical or a named logical vector
            if (!is.null(names(layer$show.legend))) {
                layer$show.legend <- rename_aes(layer$show.legend)
                include <- is.na(layer$show.legend[matched]) ||
                    layer$show.legend[matched]
            } else {
                include <- is.na(layer$show.legend) || layer$show.legend
            }

            if (include) {
                # Default is to include it

                # Filter out set aesthetics that can't be applied to the legend
                n <- vapply(layer$aes_params, length, integer(1))
                params <- layer$aes_params[n == 1]

                data <- layer$geom$use_defaults(guide$key[matched], params)
            } else {
                return(NULL)
            }
        } else {
            # This layer does not contribute to the legend
            if (is.na(layer$show.legend) || !layer$show.legend) {
                # Default is to exclude it
                return(NULL)
            } else {
                data <- layer$geom$use_defaults(NULL, layer$aes_params)[rep(1, nrow(guide$key)), ]
            }
        }

        # override.aes in guide_legend manually changes the geom
        data <- utils::modifyList(data, guide$override.aes)

        list(
            draw_key = layer$geom$draw_key,
            data = data,
            params = c(layer$geom_params, layer$stat_params)
        )
    })

    # remove null geom
    guide$geoms <- plyr::compact(guide$geoms)

    # Finally, remove this guide if no layer is drawn
    if (length(guide$geoms) == 0) guide <- NULL
    guide
}

#' @export
guide_gengrob.vector <- function(guide, theme) {

    # default setting
    label.position <- guide$label.position %||% "right"
    if (!label.position %in% c("top", "bottom", "left", "right"))
        stop("label position \"", label.position, "\" is invalid")

    nbreak <- nrow(guide$key)

    grob.title <- ggname("guide.title",
                         element_grob(
                             guide$title.theme %||% calc_element("legend.title", theme),
                             label = guide$title,
                             hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
                             vjust = guide$title.vjust %||% 0.5,
                             margin_x = FALSE,
                             margin_y = FALSE
                         )
    )

    title_width <- width_cm(grob.title)
    title_height <- height_cm(grob.title)

    # gap between keys etc
    hgap <- width_cm(theme$legend.spacing.x  %||% unit(0.3, "line"))
    vgap <- height_cm(theme$legend.spacing.y %||% (0.5 * unit(title_height, "cm")))

    # Labels
    if (!guide$label || is.null(guide$key$.label)) {
        grob.labels <- rep(list(zeroGrob()), nrow(guide$key))
    } else {
        label.theme <- guide$label.theme %||% calc_element("legend.text", theme)

        hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
            if (any(is.expression(guide$key$.label))) 1 else 0
        vjust <- y <- guide$label.vjust %||% 0.5

        grob.labels <- lapply(guide$key$.label, function(label, ...) {
            g <- element_grob(
                element = label.theme,
                label = label,
                x = x,
                y = y,
                hjust = hjust,
                vjust = vjust,
                margin_x = FALSE,
                margin_y = FALSE
            )
            ggname("guide.label", g)
        })
    }

    label_widths <- width_cm(grob.labels)
    label_heights <- height_cm(grob.labels)

    # Keys
    key_width <- width_cm(
        guide$keywidth %||% theme$legend.key.width %||% theme$legend.key.size
    )
    key_height <- height_cm(
        guide$keyheight %||% theme$legend.key.height %||% theme$legend.key.size
    )

    key_size_mat <- do.call("cbind", lapply(guide$geoms, function(g) g$data$size / 10))
    if (nrow(key_size_mat) == 0 || ncol(key_size_mat) == 0) {
        key_size_mat <- matrix(0, ncol = 1, nrow = nbreak)
    }
    key_sizes <- apply(key_size_mat, 1, max)

    if (!is.null(guide$nrow) && !is.null(guide$ncol) &&
        guide$nrow * guide$ncol < nbreak) {
        stop(
            "`nrow` * `ncol` needs to be larger than the number of breaks",
            call. = FALSE
        )
    }

    # If neither nrow/ncol specified, guess with "reasonable" values
    if (is.null(guide$nrow) && is.null(guide$ncol)) {
        if (guide$direction == "horizontal") {
            guide$nrow <- ceiling(nbreak / 5)
        } else {
            guide$ncol <- ceiling(nbreak / 20)
        }
    }
    legend.nrow <- guide$nrow %||% ceiling(nbreak / guide$ncol)
    legend.ncol <- guide$ncol %||% ceiling(nbreak / guide$nrow)

    key_sizes <- matrix(
        c(key_sizes, rep(0, legend.nrow * legend.ncol - nbreak)),
        legend.nrow,
        legend.ncol,
        byrow = guide$byrow
    )

    key_widths <- pmax(key_width, apply(key_sizes, 2, max))
    key_heights <- pmax(key_height, apply(key_sizes, 1, max))

    label_widths <- apply(
        matrix(
            c(label_widths, rep(0, legend.nrow * legend.ncol - nbreak)),
            legend.nrow,
            legend.ncol,
            byrow = guide$byrow
        ),
        2,
        max
    )
    label_heights <- apply(
        matrix(
            c(label_heights, rep(0, legend.nrow * legend.ncol - nbreak)),
            legend.nrow,
            legend.ncol,
            byrow = guide$byrow
        ),
        1,
        max
    )

    if (guide$byrow) {
        vps <- data.frame(
            R = ceiling(seq(nbreak) / legend.ncol),
            C = (seq(nbreak) - 1) %% legend.ncol + 1
        )
    } else {
        vps <- as.data.frame(arrayInd(seq(nbreak), dim(key_sizes)))
        names(vps) <- c("R", "C")
    }

    # layout of key-label depends on the direction of the guide
    if (guide$byrow == TRUE) {
        switch(
            label.position,
            "top" = {
                kl_widths <- pmax(label_widths, key_widths)
                kl_heights <- utils::head(
                    interleave(label_heights, vgap / 2, key_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 1,
                    key.col = C,
                    label.row = R * 4 - 3,
                    label.col = C
                )
            },
            "bottom" = {
                kl_widths <- pmax(label_widths, key_widths)
                kl_heights <- utils::head(
                    interleave(key_heights, vgap / 2, label_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 3,
                    key.col = C,
                    label.row = R * 4 - 1,
                    label.col = C
                )
            },
            "left" = {
                kl_widths <- utils::head(
                    interleave(label_widths, hgap / 2, key_widths, hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(pmax(label_heights, key_heights), vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 2 - 1,
                    key.col = C * 4 - 1,
                    label.row = R * 2 - 1,
                    label.col = C * 4 - 3
                )
            },
            "right" = {
                kl_widths <- utils::head(
                    interleave(key_widths, hgap / 2, label_widths, hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(pmax(label_heights, key_heights), vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 2 - 1,
                    key.col = C * 4 - 3,
                    label.row = R * 2 - 1,
                    label.col = C * 4 - 1
                )
            })
    } else {
        switch(
            label.position,
            "top" = {
                kl_widths <- utils::head(
                    interleave(pmax(label_widths, key_widths), hgap/2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(label_heights, vgap / 2, key_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 1,
                    key.col = C * 2 - 1,
                    label.row = R * 4 - 3,
                    label.col = C * 2 - 1
                )
            },
            "bottom" = {
                kl_widths <- utils::head(
                    interleave(pmax(label_widths, key_widths), hgap / 2),
                    -1
                )
                kl_heights <- utils::head(
                    interleave(key_heights, vgap / 2, label_heights, vgap / 2),
                    -1
                )
                vps <- transform(
                    vps,
                    key.row = R * 4 - 3,
                    key.col = C * 2 - 1,
                    label.row = R * 4 - 1,
                    label.col = C * 2 - 1
                )
            },
            "left" = {
                kl_widths <- utils::head(
                    interleave(label_widths, hgap / 2, key_widths, hgap / 2),
                    -1
                )
                kl_heights <- pmax(key_heights, label_heights)
                vps <- transform(
                    vps,
                    key.row = R,
                    key.col = C * 4 - 1,
                    label.row = R,
                    label.col = C * 4 - 3
                )
            },
            "right" = {
                kl_widths <- utils::head(
                    interleave(key_widths, hgap / 2, label_widths, hgap / 2),
                    -1
                )
                kl_heights <- pmax(key_heights, label_heights)
                vps <- transform(
                    vps,
                    key.row = R,
                    key.col = C * 4 - 3,
                    label.row = R,
                    label.col = C * 4 - 1
                )
            })
    }

    # layout the title over key-label
    switch(guide$title.position,
           "top" = {
               widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
               heights <- c(title_height, vgap, kl_heights)
               vps <- transform(
                   vps,
                   key.row = key.row + 2,
                   key.col = key.col,
                   label.row = label.row + 2,
                   label.col = label.col
               )
               vps.title.row = 1; vps.title.col = seq_along(widths)
           },
           "bottom" = {
               widths <- c(kl_widths, max(0, title_width - sum(kl_widths)))
               heights <- c(kl_heights, vgap, title_height)
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col,
                   label.row = label.row,
                   label.col = label.col
               )
               vps.title.row = length(heights); vps.title.col = seq_along(widths)
           },
           "left" = {
               widths <- c(title_width, hgap, kl_widths)
               heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col + 2,
                   label.row = label.row,
                   label.col = label.col + 2
               )
               vps.title.row = seq_along(heights); vps.title.col = 1
           },
           "right" = {
               widths <- c(kl_widths, hgap, title_width)
               heights <- c(kl_heights, max(0, title_height - sum(kl_heights)))
               vps <- transform(
                   vps,
                   key.row = key.row,
                   key.col = key.col,
                   label.row = label.row,
                   label.col = label.col
               )
               vps.title.row = seq_along(heights); vps.title.col = length(widths)
           })

    # grob for key
    key_size <- c(key_width, key_height) * 10

    draw_key <- function(i) {
        bg <- element_render(theme, "legend.key")
        keys <- lapply(guide$geoms, function(g) {
            g$draw_key(g$data[i, ], g$params, key_size)
        })
        c(list(bg), keys)
    }
    grob.keys <- unlist(lapply(seq_len(nbreak), draw_key), recursive = FALSE)

    # background
    grob.background <- element_render(theme, "legend.background")

    ngeom <- length(guide$geoms) + 1
    kcols <- rep(vps$key.col, each = ngeom)
    krows <- rep(vps$key.row, each = ngeom)

    # padding
    padding <- grid::convertUnit(theme$legend.margin %||% ggplot2::margin(), "cm")
    widths <- c(padding[4], widths, padding[2])
    heights <- c(padding[1], heights, padding[3])

    # Create the gtable for the legend
    gt <- gtable::gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
    gt <- gtable::gtable_add_grob(
        gt,
        grob.background,
        name = "background",
        clip = "off",
        t = 1,
        r = -1,
        b = -1,
        l = 1
    )
    gt <- gtable::gtable_add_grob(
        gt,
        grob.title,
        name = "title",
        clip = "off",
        t = 1 + min(vps.title.row),
        r = 1 + max(vps.title.col),
        b = 1 + max(vps.title.row),
        l = 1 + min(vps.title.col)
    )
    gt <- gtable::gtable_add_grob(
        gt,
        grob.keys,
        name = paste("key", krows, kcols, c("bg", seq(ngeom - 1)), sep = "-"),
        clip = "off",
        t = 1 + krows,
        r = 1 + kcols,
        b = 1 + krows,
        l = 1 + kcols
    )
    gt <- gtable::gtable_add_grob(
        gt,
        grob.labels,
        name = paste("label", vps$label.row, vps$label.col, sep = "-"),
        clip = "off",
        t = 1 + vps$label.row,
        r = 1 + vps$label.col,
        b = 1 + vps$label.row,
        l = 1 + vps$label.col
    )
    gt
}

globalVariables(c("C", "R", "key.row", "key.col", "label.row", "label.col"))

