#' @importFrom grid grob
arrowGrob <- function(x, y, angle, length, pivot, default.units = "npc", ...) {
    # angle en grados matemático 0 a 360
    # length en alguna unidad.
    # pivot 0 a 1.
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)

    grob(x = x, y = y, angle = angle, length = length, pivot = pivot, ..., cl = "arrow2")
}

#' @importFrom grid convertX convertY unit.c
#' @export
makeContent.arrow2 <- function(x) {

    x$id <- rep(seq(length(x$x)), 2)
    x$x <- convertX(x$x, 'mm')
    x$y <- convertY(x$y, 'mm')
    dx <- x$length*cos(x$angle*pi/180)
    dy <- x$length*sin(x$angle*pi/180)

    x$x <- unit.c(x$x - dx*x$pivot,
                        x$x + dx*(1-x$pivot))

    x$y <- unit.c(x$y - dy*x$pivot,
                        x$y + dy*(1-x$pivot))


    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

#' @importFrom grid grob
vectorGrob <- function(x, y, dx, dy, length, preserve.dir,
                       default.units = "npc", pivot, ...) {
    # angle <- atan2(yend - y, xend - x)
    x <- .unit_ifnot(x, default.units)
    y <- .unit_ifnot(y, default.units)
    dx <- .unit_ifnot(dx, default.units)
    dy <- .unit_ifnot(dy, default.units)

    grob(x = x, y = y, dx = dx, dy = dy, length = length, pivot = pivot,
               preserve.dir = preserve.dir, ..., cl = "vector")
}

#' @importFrom grid convertX convertY convertWidth convertHeight unit.c
#' @export
makeContent.vector <- function(x) {
    x$id <- rep(seq(length(x$x)), 2)
    x$x <- convertX(x$x, 'mm')
    x$y <- convertY(x$y, 'mm')
    x$dx <- convertWidth(x$dx, 'mm')
    x$dy <- convertHeight(x$dy, 'mm')

    x$angle <- atan2(as.numeric(x$dy), as.numeric(x$dx))

    x$dx <- x$length*cos(x$angle)
    x$dy <- x$length*sin(x$angle)

    x$x <- unit.c(x$x - x$dx*x$pivot,
                        x$x + x$dx*(1 - x$pivot))

    x$y <- unit.c(x$y - x$dy*x$pivot,
                        x$y + x$dy*(1 - x$pivot))

    x$cl <- "polyline"
    class(x)[1] <- "polyline"
    x
}

#' @importFrom grid is.unit unit
.unit_ifnot <- function(x, unit) {
    if (!is.unit(x)) {
        x <- unit(x, unit)
    }
    x
}

