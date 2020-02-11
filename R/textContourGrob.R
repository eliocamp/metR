# from shadowtext
shadowtext <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                       just = "centre", hjust = NULL, vjust = NULL,
                       dx = 0, dy = 0,
                       check.overlap = FALSE,
                       default.units = "npc", name = NULL, gp = grid::gpar(col = "white"),
                       vp = NULL, bg.color = "black", bg.r = 0.1) {
    upperGrob <- textContourGrob(label = label, x = x, y = y, just = just,
                                 hjust = hjust, vjust = vjust,
                                 dx = dx, dy = dy,
                                 default.units = default.units,
                                 check.overlap = check.overlap, name = name, gp = gp,
                                 vp = vp)
    if (is.null(bg.color))
        return(upperGrob)
    gp$col <- bg.color
    theta <- seq(pi/8, 2 * pi, length.out = 16)
    char <- substring(label[1], 1, 1)
    r <- bg.r[1]
    if (r != 0) {
        bgList <- lapply(theta, function(i) {
            if (!grid::is.unit(x))
                x <- grid::unit(x, default.units)
            if (!grid::is.unit(y))
                y <- grid::unit(y, default.units)
            x <- x + grid::unit(cos(i) * r, "strwidth", data = char)
            y <- y + grid::unit(sin(i) * r, "strheight", data = char)
            textContourGrob(label = label, x = x, y = y, just = just, hjust = hjust,
                            vjust = vjust,
                            dx = dx, dy = dy,
                            default.units = default.units,
                            check.overlap = check.overlap, name = name, gp = gp,
                            vp = vp)
        })
    } else {
        bgList <- list()
    }
    bgGrob <- do.call(grid::gList, bgList)
    grobs <- grid::gList(bgGrob, upperGrob)
    grid::gTree(children = grobs)
}



textContourGrob <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                            just = "centre", hjust = NULL, vjust = NULL,
                            dx = 0, dy = 0,
                            bg.r = 0.1, bg.color = "black",
                            check.overlap = FALSE, default.units = "npc",
                            name = NULL, gp = grid::gpar(), vp = NULL) {
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)

    dx <- .unit_ifnot(dx, default.units)
    dy <- .unit_ifnot(dy, default.units)

    grid::grob(label = label, x = x, y = y, just = just, hjust = hjust,
         vjust = vjust,
         dx = dx, dy = dy,
         bg.r = bg.r, bg.color = bg.color,
         check.overlap = check.overlap,
         name = name, gp = gp, vp = vp, cl = "textContour")
}

#' @export
#' @importFrom grid makeContent
makeContent.textContour <- function(x) {
    x$x <- grid::convertX(x$x, 'mm')
    x$y <- grid::convertY(x$y, 'mm')
    x$dx <- grid::convertWidth(x$dx, 'mm')
    x$dy <- grid::convertHeight(x$dy, 'mm')

    x$rot <- atan2(as.numeric(x$dy), as.numeric(x$dx))*180/pi

    x$rot <- ifelse(x$rot > 180, x$rot - 180, x$rot)
    x$rot <- ifelse(x$rot > 90, x$rot - 180, x$rot)
    x$rot <- ifelse(x$rot < -90, x$rot + 180, x$rot)

    x$cl <- "text"
    class(x)[1] <- "text"
    x
}
