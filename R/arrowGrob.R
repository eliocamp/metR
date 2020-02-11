#' @importFrom grid grob
arrowGrob <- function(x, y, angle, length, pivot, default.units = "npc", ...) {
    # angle en grados matemático 0 a 360
    # length en alguna unidad.
    # pivot 0 a 1.
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)

    grid::grob(x = x, y = y, angle = angle, length = length, pivot = pivot, ..., cl = "arrow2")
}

#' @export
#' @importFrom grid makeContent
makeContent.arrow2 <- function(x) {
# browser()
    x$id <- rep(seq(length(x$x)), 2)
    x$x <- grid::convertX(x$x, 'mm')
    x$y <- grid::convertY(x$y, 'mm')
    dx <- grid::convertUnit(x$length, "mm")*cos(x$angle*pi/180)
    dy <- grid::convertUnit(x$length, "mm")*sin(x$angle*pi/180)

    x$x <- fast.unit.c(x$x - dx*x$pivot,
                        x$x + dx*(1-x$pivot))

    x$y <- fast.unit.c(x$y - dy*x$pivot,
                        x$y + dy*(1-x$pivot))


    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

fast.unit.c <- memoise::memoise(grid::unit.c)
# fast.unit.c <- grid::unit.c

# fast.unit.c <- function(...) {
#     browser()
#     x <- list(...)
#     unit <- x[[1]]
#
#     x <- c(...)
#
#     grid::unit(x, unit)
# }

vectorGrob <- function(x, y, dx, dy, length, preserve.dir,
                       default.units = "npc", pivot, ...) {
    # angle <- atan2(yend - y, xend - x)
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)
    dx <- .unit_ifnot(dx, default.units)
    dy <- .unit_ifnot(dy, default.units)

    grid::grob(x = x, y = y, dx = dx, dy = dy, length = length, pivot = pivot,
               preserve.dir = preserve.dir, ..., cl = "vector")
}

#' @export
#' @importFrom grid makeContent
makeContent.vector <- function(x) {
    x$id <- rep(seq(length(x$x)), 2)
    x$x <- grid::convertX(x$x, 'mm')
    x$y <- grid::convertY(x$y, 'mm')
    x$dx <- grid::convertWidth(x$dx, 'mm')
    x$dy <- grid::convertHeight(x$dy, 'mm')

    x$angle <- atan2(as.numeric(x$dy), as.numeric(x$dx))

    x$dx <- x$length*cos(x$angle)
    x$dy <- x$length*sin(x$angle)

    x$x <- grid::unit.c(x$x - x$dx*x$pivot,
                        x$x + x$dx*(1 - x$pivot))

    x$y <- grid::unit.c(x$y - x$dy*x$pivot,
                        x$y + x$dy*(1 - x$pivot))

    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

.unit_ifnot <- function(x, unit) {
    if (!grid::is.unit(x)) {
        x <- grid::unit(x, unit)
    }
    x
}

