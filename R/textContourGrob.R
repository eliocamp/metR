# from shadowtext
#' @importFrom grid gList gTree
shadowtext <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                       just = "centre", hjust = NULL, vjust = NULL,
                       dx = 0, dy = 0,
                       check.overlap = FALSE,
                       default.units = "npc", name = NULL, gp = gpar(col = "white"),
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
            if (!is.unit(x))
                x <- unit(x, default.units)
            if (!is.unit(y))
                y <- unit(y, default.units)
            x <- x + unit(cos(i) * r, "strwidth", data = char)
            y <- y + unit(sin(i) * r, "strheight", data = char)
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
    bgGrob <- do.call(gList, bgList)
    grobs <- gList(bgGrob, upperGrob)
    gTree(children = grobs)
}


#' @importFrom grid grob
textContourGrob <- function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                            just = "centre", hjust = NULL, vjust = NULL,
                            dx = 0, dy = 0,
                            bg.r = 0.1, bg.color = "black",
                            check.overlap = FALSE, default.units = "npc",
                            name = NULL, gp = gpar(), vp = NULL) {
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)

    dx <- .unit_ifnot(dx, default.units)
    dy <- .unit_ifnot(dy, default.units)

    grob(label = label, x = x, y = y, just = just, hjust = hjust,
         vjust = vjust,
         dx = dx, dy = dy,
         bg.r = bg.r, bg.color = bg.color,
         check.overlap = check.overlap,
         name = name, gp = gp, vp = vp, cl = "textContour")
}

#' @importFrom grid convertX convertY convertWidth convertHeight unit.c
#' @export
makeContent.textContour <- function(x) {
    x$x <- convertX(x$x, 'mm')
    x$y <- convertY(x$y, 'mm')
    x$dx <- convertWidth(x$dx, 'mm')
    x$dy <- convertHeight(x$dy, 'mm')

    x$rot <- atan2(as.numeric(x$dy), as.numeric(x$dx))*180/pi

    x$rot <- ifelse(x$rot > 180, x$rot - 180, x$rot)
    x$rot <- ifelse(x$rot > 90, x$rot - 180, x$rot)
    x$rot <- ifelse(x$rot < -90, x$rot + 180, x$rot)

    x$cl <- "text"
    class(x)[1] <- "text"
    x
}
