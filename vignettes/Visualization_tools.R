## ---- message = FALSE, fig.width = 7-------------------------------------
library(meteoR)
library(ggplot2)
library(data.table)
nceptemperature <- copy(nceptemperature)
nceptemperature[, air.z := Anomaly(air), by = .(lat, lev)]

# Plot made with base ggplot
g <- ggplot(nceptemperature[lon %~% 180], aes(lat, lev, z = air.z)) +
    geom_contour(aes(color = ..level..))

g

## ---- fig.width = 7------------------------------------------------------
g + 
    scale_y_level() +
    scale_x_latitude(ticks = 15) +
    scale_color_divergent()

## ---- fig.show='hold', fig.width = 3.3-----------------------------------
breaks = seq(100, 200, by = 10)
ggplot(melt(volcano), aes(Var1, Var2, z = value)) +
    stat_contour(aes(fill = ..level..), geom = "polygon", breaks = breaks) +
    geom_contour(color = "red", size = 0.2, breaks = 150) +
    geom_contour(color = "blue", size = 0.2, breaks = 160)

ggplot(melt(volcano), aes(Var1, Var2, z = value)) +
    stat_contour_fill(breaks = breaks)

## ----  fig.width = 7-----------------------------------------------------
g <- ggplot(nceptemperature[lev == 300], aes(lon, lat, z = air.z)) +
    stat_contour_fill(exclude = 0) +
    scale_fill_divergent() +
    scale_x_longitude() +
    scale_y_latitude()
g

## ----  fig.width = 7-----------------------------------------------------
nceptemperature[, c("t.dx", "t.dy") := Derivate(air.z ~ lon + lat, 
                                               bc = c("cyclic", "none"), 
                                               sphere = T), 
                by = lev]

ggplot(nceptemperature[lev == 500], aes(lon, lat)) +
    stat_contour_fill(aes(z = air.z)) +
    geom_arrow(aes(dx = t.dx, dy = t.dy), skip = c(2, 1), scale = 5e5, min.mag = 2e-6,
               arrow.size = 0.3) +
    scale_y_latitude(limits = c(-90, 0)) +
    scale_x_longitude()

## ---- fig.show='hold', fig.width = 3.3, warning = FALSE------------------
g <- ggplot(RepeatLon(nceptemperature[lev == 500]), aes(lon, lat)) +
    stat_contour_fill(aes(z = air.z)) +
    scale_y_latitude(limits = c(-80, 0)) +
    scale_x_longitude() +
    scale_fill_divergent(guide = "none") +
    coord_map()

g 
g + geom_arrow(aes(dx = t.dx, dy = t.dy), skip = c(2, 2), scale = 5e5, min.mag = 2e-6,
               arrow.size = 0.3) 

## ---- fig.width = 3.3, fig.show = 'hold', warning = FALSE----------------
g <- ggplot(nceptemperature[lev == 300], aes(lon, lat, z = air.z)) +
    geom_contour(binwidth = 1.5) +
    scale_fill_divergent() +
    scale_x_longitude() +
    scale_y_latitude(limits = c(-90, -20)) +
    coord_polar()

g

g %+% RepeatLon(nceptemperature[lev == 300]) 

