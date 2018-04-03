#' @importFrom stats rnorm
#' @importFrom akima interpp
streamline <- function(field, dt = 0.1, S = 3, skip.x = 1, skip.y = 1) {
    #
    # dt <- 0.2
    # S <- 10
    # skip.x <- 1
    # skip.y <- 1
    #
    force.fun <- function(x, y, field) {
        force <- as.data.table(
            suppressWarnings(akima::interpp(field$x, field$y, xo = x, yo = y, z = field$dx)))
        colnames(force)[3] <- "dx"
        dy <- as.data.table(
            suppressWarnings(akima::interpp(field$x, field$y, xo = x, yo = y, z = field$dy)))
        force$dy <- dy$z
        return(force[, .(dx, dy)])
    }

    field <- field[!is.na(dx) & !is.na(dy)]

    points <- data.table::copy(field)
    points <- points[dx + dy != 0]
    points <- subset(points,
                     x %in% JumpBy(unique(x), skip.x + 1) &
                         y %in% JumpBy(unique(y), skip.y + 1))

    rx <- ggplot2::resolution(points$x, zero = FALSE)
    ry <- ggplot2::resolution(points$y, zero = FALSE)
    set.seed(42)
    points[, x := x + rnorm(.N, rx, rx)]
    points[, y := y + rnorm(.N, ry, ry)]

    range.x <- range(field$x)
    range.y <- range(field$y)
    points <- points[x %between% range.x & y %between% range.y]

    points[, point := 1:.N]
    points[, sim := 0]
    # points[, c("dx", "dy") := force.fun(x, y, field)]
    points2 <- copy(points)

    for (s in 1:S) {
        points2[, c("x", "y", "sim") := .(x + dx*dt, y + dy*dt, s)]
        points2 <- points2[x %between% range.x & y %between% range.y]
        points2[, c("dx", "dy") := force.fun(x, y, field)]
        points <- rbind(points, points2)  # se puede optimizar prealocando
    }
    return(points)
}


dx <- function(dlon, dx, lat, a = 6731000) {
    if (hasArg(dlon) & !hasArg(dx)) {
        return(dlon*pi/180*a*cos(lat*pi/180))
    } else if (!hasArg(dlon) & hasArg(dx)) {
        return(dx/(a*cos(lat*pi/180))*180/pi)
    }
}

dy <- function(dlat, dy, a = 6731000) {
    if (hasArg(dlat) & !hasArg(dy)) {
        return(dlat*a*pi/180)
    } else if (!hasArg(dlat) & hasArg(dy)) {
        return(dy/a*180/pi)
    }
}


# Working example
#
# TODO:
#    - alguna forma de determinar el dt y el S de manera automática
#    - Ver qué pasa que algunas flechas tienen algo raro en el primer paso
#    - soporte para coordenadas cíclicas
#    - prealocar el data.frame
#
# library(metR)
# library(data.table)
# library(ggplot2)
#
# data(aao)
# v <- aao[date == date[1]]
# v[, gh.z := Anomaly(gh), by = .(lat)]
# v[, c("dx", "dy") := GeostrophicWind(gh.z, lon, lat)]
#
# a <- 6731000
# field <- v[, .(x = lon, y = lat, dx, dy)]
# field[, dy := dy(dy = dy)]
# field[, dx := dx(dx = dx, lat = y)]
#
# # Una forma posible de estimar el dt.
# # Problema: asume que x, y, dx, dy están en las mismas
# # unidades.
# M <- mean(field[, Mag(dx, dy)], na.rm = T)
# res <- min(ggplot2::resolution(field$x, zero = FALSE),
#            ggplot2::resolution(field$y, zero = FALSE))
# dt <- res/M*0.5
#
#
# points <- streamline(field, S = 20, dt = dt)
#
# ggplot(points, aes(x, y)) +
#     geom_contour_fill(data = v, aes(lon, lat, z = gh.z)) +
#     geom_path(aes(group = point), arrow = grid::arrow(13, unit(0.3, "lines"))) +
#     scale_fill_divergent(name = "GH") +
#     coord_quickmap()



