# from shadowtext
contourTextGrob <- function(label, type = "text", x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                       group = rep(1, length(label)),
                       just = "centre", hjust = NULL, vjust = NULL,
                       check.overlap = FALSE,
                       rotate = TRUE,
                       default.units = "npc", name = NULL,

                       col = "white",
                       fontsize = NULL,
                       fontfamily = NULL,
                       fontface = NULL,
                       lineheight = NULL,

                       # for labels
                       label.padding =  grid::unit(0.25, "lines"),
                       label.r = grid::unit(0.15, "lines"),
                       label.size = 0.25,
                       fill = "white",   # aes
                       alpha = 1,        # aes


                       # gp = grid::gpar(col = "white"),
                       vp = NULL, bg.color = "black", bg.r = 0.1,
                       position = label_placer_flattest()) {


    if (!grid::is.unit(x)) {
        x <- grid::unit(x, default.units)
    }

    if (!grid::is.unit(y)) {
        y <- grid::unit(y, default.units)
    }



    objects <- lapply(unique(group), function(g) {
        group <- group == g

        list(label = unique(label[group]),
             x = x[group], y = y[group],
             just = just,
             type = type,
             hjust = hjust, vjust = vjust,
             default.units = default.units,
             check.overlap = check.overlap,
             name = name,
             position = position,
             rotate = rotate,
             bg.r = bg.r,
             bg.color = bg.color[group],

             label.padding = label.padding,
             label.r = label.r,
             label.size = label.size,
             alpha = alpha[group],
             fill = fill[group],


             # gp = grid::gpar(
             col = col[group][1],
             fontsize = fontsize[group][1],
             fontfamily = fontfamily[group][1],
             fontface = fontface[group][1],
             lineheight = lineheight[group][1],
             # ),
             vp = vp)
    })

    grid::gTree(data = objects, cl = "contourTextGrob")

}

#' @export
makeContent.contourTextGrob <- function(x) {
    payload <- x


    data <- lapply(x$data, content_grob)
    valid <- lengths(data) != 0
    data <- data[valid]

    label <- unlist(lapply(data, function(x) x$label))
    x_coord <- Reduce(grid::unit.c, lapply(data, function(x) x$x))
    y <- Reduce(grid::unit.c, lapply(data, function(x) x$y))
    col <- unlist(lapply(data, function(x) x$col))
    fontsize <- unlist(lapply(data, function(x) x$fontsize))
    fontfamily <- unlist(lapply(data, function(x) x$fontfamily))
    fontface <- unlist(lapply(data, function(x) x$fontface))
    lineheight <- unlist(lapply(data, function(x) x$lineheight))
    rot <- unlist(lapply(data, function(x) x$rot))
    bg.color <-  unlist(lapply(data, function(x) x$bg.color))


    just <-  unlist(lapply(payload$data, function(x) x$just))[valid]
    hj <- unlist(lapply(payload$data, function(x)  grid::resolveHJust(x$just, NULL)))[valid]
    vj <- unlist(lapply(payload$data, function(x)  grid::resolveVJust(x$just, NULL)))[valid]

    default.units <-  unlist(lapply(payload$data, function(x) x$default.units))[1]
    check.overlap <-  unlist(lapply(payload$data, function(x) x$check.overlap))[1]
    bg.r <-  unlist(lapply(payload$data, function(x) x$bg.r))[1]



    if (payload$data[[1]]$type == "label") {

        label.r <- data[[1]]$label.r
        label.size <- data[[1]]$label.size
        label.padding <- data[[1]]$label.padding
        fill <-  vapply(data, function(x) x$fill, character(1))
        alpha <- vapply(data, function(x) x$alpha, numeric(1))
        grobs <- lapply(seq_along(label), function(i) {

            labelGrob(label[i],
                      x = x_coord[i],
                      y = y[i],
                      just = c(hj[i], vj[i]),

                      padding = label.padding,
                      r = label.r,

                      text.gp = grid::gpar(
                          col = col[i],
                          fontsize = fontsize[i],
                          fontfamily = fontfamily[i],
                          fontface = fontface[i],
                          lineheight = lineheight[i]
                      ),
                      rect.gp = grid::gpar(
                          col = if (isTRUE(all.equal(label.size, 0))) NA else col[i],
                          fill = scales::alpha(fill[i], alpha[i]),
                          lwd = label.size * .pt
                      )
            )
        })

        class(grobs) <- "gList"

        grobs <- ggname("geom_label", grid::grobTree(children = grobs))
        return(grobs)
    }


    grob <- shadowtextGrob(label, x_coord, y, just = c(hj, vj),
                           default.units = default.units,
                           check.overlap = check.overlap,
                           bg.r = bg.r,
                           rot = rot,
                           bg.colour = bg.color,
                           gp = grid::gpar(
                               col = col,
                               fontsize = fontsize,
                               fontfamily = fontfamily,
                               fontface = fontface,
                               lineheight = lineheight)
    )



    grid::setChildren(x, grid::gList(grob))
}




content_grob <- function(x) {
    x_coord <- grid::convertX(x$x, unitTo = "mm", valueOnly = TRUE)
    y_coord <- grid::convertY(x$y, unitTo = "mm", valueOnly = TRUE)

    id <- seq_along(y_coord)
    dx <- .derv(x_coord, id, fill = TRUE)
    dy <- .derv(y_coord, id, fill = TRUE)

    lines <- list(list(x = x_coord, y = y_coord, id = rep(1, length(x_coord))))
    names(lines) <- x$label
    labels_data <- data.frame(index = 1,
                              break_index = 1,
                              break_id = x$label,
                              label = x$label)
    locations <- x$position(lines, labels_data)

    selected <- which(x_coord %in% locations$x & y_coord %in% locations$y)

    if (nrow(locations) == 0) {
        return(list())
    }

    if (x$rotate == TRUE) {
        rot <- text_angle(dx, dy)
    } else {
        rot <- rep(0, length(x_coord))
    }

    # from shadowtext
    list(
        label = rep(x$label, length = nrow(locations)),
        x = x$x[selected], y = x$y[selected],
        col = x$col,
        rot = rot[selected],
        bg.color = x$bg.color[selected],
        fontsize = x$fontsize,
        fontfamily = x$fontfamily,
        fontface = x$fontface,
        lineheight = x$lineheight,
        label.padding = x$label.padding,
        label.size = x$label.size,
        label.r = x$label.r,
        alpha = x$alpha[selected],
        fill = x$fill[selected]
    )
}

text_angle <- function(dx, dy) {
    angle <- atan2(dy, dx)*180/pi
    angle <- ifelse(angle > 180, angle - 180, angle)
    angle <- ifelse(angle > 90, angle - 180, angle)
    angle <- ifelse(angle < -90, angle + 180, angle)
    angle
}


# From ggplot2
labelGrob <- function (label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
          just = "center", padding = grid::unit(0.25, "lines"), r = grid::unit(0.1,
                                                                   "snpc"),
          default.units = "npc", name = NULL, text.gp = grid::gpar(),
          rect.gp = grid::gpar(fill = "white"), vp = NULL) {
    if (length(label) != 1) {
        stopf("label must be of length 1")
    }
    if (!grid::is.unit(x))
        x <- grid::unit(x, default.units)
    if (!grid::is.unit(y))
        y <- grid::unit(y, default.units)
    grid::gTree(label = label, x = x, y = y, just = just, padding = padding,
          r = r, name = name, text.gp = text.gp, rect.gp = rect.gp,
          vp = vp, cl = "metR_labelgrob")
}

#' @export
makeContent.metR_labelgrob <- function(x) {
    hj <- grid::resolveHJust(x$just, NULL)
    vj <- grid::resolveVJust(x$just, NULL)

    t <- grid::textGrob(
        x$label,
        x$x + 2 * (0.5 - hj) * x$padding,
        x$y + 2 * (0.5 - vj) * x$padding,
        just = c(hj, vj),
        gp = x$text.gp,
        name = "text"
    )

    r <- grid::roundrectGrob(
        x$x, x$y, default.units = "native",
        width = grid::grobWidth(t) + 2 * x$padding,
        height = grid::grobHeight(t) + 2 * x$padding,
        just = c(hj, vj),
        r = x$r,
        gp = x$rect.gp,
        name = "box"
    )

    grid::setChildren(x, grid::gList(r, t))
}

ggname <- function (prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}



# from shadowtext
shadowtextGrob <- function (label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                            just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE,
                            default.units = "npc", name = NULL, gp = grid::gpar(col = "white"),
                            vp = NULL, bg.colour = "black", bg.r = 0.1)
{
    upperGrob <- grid::textGrob(label = label, x = x, y = y, just = just,
                                hjust = hjust, vjust = vjust, rot = rot, default.units = default.units,
                                check.overlap = check.overlap, name = name, gp = gp,
                                vp = vp)
    if (bg.r == 0)
        return(upperGrob)
    gp$col <- bg.colour
    theta <- seq(pi/8, 2 * pi, length.out = 16)
    char <- "X"
    r <- bg.r[1]
    bgList <- lapply(theta, function(i) {
        if (!grid::is.unit(x))
            x <- grid::unit(x, default.units)
        if (!grid::is.unit(y))
            y <- grid::unit(y, default.units)
        x <- x + grid::unit(cos(i) * r, "strheight", data = char)
        y <- y + grid::unit(sin(i) * r, "strheight", data = char)
        grid::textGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                       vjust = vjust, rot = rot, default.units = default.units,
                       check.overlap = check.overlap, name = name, gp = gp,
                       vp = vp)
    })
    bgGrob <- do.call(grid::gList, bgList)
    grobs <- grid::gList(bgGrob, upperGrob)
    grid::gTree(children = grobs)
}
