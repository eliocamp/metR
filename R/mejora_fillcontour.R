library(data.table)
library(ggplot2)





.TrimField <- function(x, M, m) {
    ifelse(x > M, M,
           ifelse(x < m, m, x))
}


.IntersectSegments <- function(s1, s2){
    # from https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
    # the segments s1 and s2 are lists with elements x and y: numeric vectors
    # of length 2.
    x1 <- s1$x[1]
    y1 <- s1$y[1]

    x2 <- s1$x[2]
    y2 <- s1$y[2]

    x3 <- s2$x[1]
    y3 <- s2$y[1]

    x4 <- s2$x[2]
    y4 <- s2$y[2]

    denom <- ((y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1))
    denom[abs(denom) < 1e-10] <- NA # parallel lines

    ua <- ((x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3)) / denom
    ub <- ((x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3)) / denom

    x <- x1 + ua * (x2 - x1)
    y <- y1 + ua * (y2 - y1)
    inside <- (ua >= 0) & (ua <= 1) & (ub >= 0) & (ub <= 1)
    data.frame(x = ifelse(inside, x, NA),
               y = ifelse(inside, y, NA))

}


.GetGridPoints <- function(grid) {
    grid <- as.matrix(grid[, c("x", "y")])
    triangles <- grid[grDevices::chull(grid), ]
    as.data.frame(triangles)
}




data <- as.data.table(expand.grid(x = 1:10, y = 1:10))
data <- data[x < y]
data[, z := x + y]
m <- mean(data$z)
dx = 1
dy = 1
data.extra <- copy(data)
data.extra.x <- data.extra[ ,  .(x = c(max(x) + dx, min(x) - dx), z = m), by = y]

data.extra <- rbind(data.extra, data.extra.x)
data.extra.y <- data.extra[ ,  .(y = c(max(y) + dy, min(y) - dy), z = m), by = x]
data.extra   <- rbind(data.extra, data.extra.y)

data.extra[x %b% c(2, 9), M := max(x), by  = .(y)]
data.extra <- data.extra[x < M | is.na(M)]


breaks <- pretty(range(data$z), 10)
ggplot(data, aes(x, y)) +
    geom_contour(aes(z = z)) +
    geom_point(data = data.extra, color = "red") +
    geom_point(size = 2) +
    geom_contour(data = data.extra, aes( z = z), breaks = breaks)


    cont <- as.data.table(ggplot2:::contour_lines(data.extra, breaks, complete = TRUE))
cont.bk <- copy(cont)
bounds.x <- data[, .(M = max(x), m = min(x)), by = .(ybound = y)]
bounds.y <- data[, .(M = max(y), m = min(y)), by = .(xbound = x)]

ggplot(cont, aes(x, y)) +
    geom_path(aes(group = piece)) +
    geom_point() +
    geom_point(data = data, color = "red") +
    geom_point(data = cont[x %in% x.un], color = "blue")

x.un <- unique(data$x)
y.un <- unique(data$y)



cont[x %in% x.un,
     y := .TrimField(y, bounds.y[xbound == x, M], bounds.y[xbound == x, m]),
     by = x]

cont[y %in% y.un,
     x := .TrimField(x, bounds.x[ybound == y, M], bounds.x[ybound == y, m]),
     by = y]

ggplot(cont.bk, aes(x, y)) +
    geom_polygon(aes(fill = level, group = piece)) +
    geom_path(aes(group = piece), data = cont.bk) +
    geom_point() +
    geom_point(data = data, color = "red")
