library(metR)
library(ggplot2)
library(magrittr)
library(data.table)

subset <- list(level = 925,
               time = "2000-01-01")
geopotential <- ReadNetCDF("~/DATOS/NCEP Reanalysis/air.mon.mean.nc", c(t = "air"),
                           subset = subset)
geopotential[, u := ReadNetCDF("~/DATOS/NCEP Reanalysis/uwnd.mon.mean.nc", c(u = "uwnd"),
                               out = "vector",
                               subset = subset)]

geopotential[, v := ReadNetCDF("~/DATOS/NCEP Reanalysis/vwnd.mon.mean.nc", c(v = "vwnd"),
                               out = "vector",
                               subset = subset)]
geopotential[, lon := ConvertLongitude(lon, from = 360)]
lons <- c(0, 90)

geopotential %>%
    # .[, t := Anomaly(t), by = .(lat)] %>%
    ggperiodic::periodic(lon = c(-180, 180)) %>%
    ggplot(aes(lon, lat)) +
    geom_contour_fill(aes(z = t), breaks = AnchorBreaks(0, 10)) +
    # geom_vector(aes(dx = dlon(u, lat), dy = dlat(v)), skip = 2,
    # data = geopotential[lat > -80]) +
    # scale_mag(guide = "none") +
    geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v),
                        size = after_stat(step)^2),
                    # min.L = 1,
                    dt = 36000/4, S = 40, lineend = "round",
                    skip = 5, xwrap = c(-180, 180),
                    arrow = NULL, data = geopotential[]) +
    scale_size(range = c(0, 1), guide = "none") +
    # geom_contour2(aes(z = t))
    # geom_path(data = map_data("world"), aes(long, lat, group = group),
    # size = 0.4, color = "gray30") +
    scale_x_longitude(limits = c(-180, 180)) +
    scale_y_latitude(limits = c(10, 90)) +
    ggalt::coord_proj(paste0("+proj=laea")) +
    # coord_polar(start = -180*pi/180) +
    theme_void() +
    scale_fill_divergent(midpoint = 0) +
    # scale_fill_divergent() +
    guides(fill = "none") +
    NULL

ggsave("logo/cap.svg")


geopotential %>%
    # .[, t := Anomaly(t), by = .(lat)] %>%
    ggperiodic::periodic(lon = c(-180, 180)) %>%
    ggplot(aes(lon, lat)) +
    geom_contour_fill(aes(z = t), breaks = 0) +
    # geom_vector(aes(dx = dlon(u, lat), dy = dlat(v)), skip = 2,
    # data = geopotential[lat > -80]) +
    # scale_mag(guide = "none") +
    # geom_streamline(aes(dx = dlon(u, lat), dy = dlat(v),
    #                     size = after_stat(step)^2),
    #                 dt = 36000/4, S = 40, lineend = "round",
    #                 skip = 5, xwrap = c(-180, 180),
    #                 arrow = NULL, data = geopotential[]) +
    # scale_size(range = c(0, 1), guide = "none") +
    # geom_contour2(aes(z = t))
    # geom_path(data = map_data("world"), aes(long, lat, group = group),
    # size = 0.4, color = "gray30") +
    scale_x_longitude(limits = c(-180, 180)) +
    scale_y_latitude(limits = c(10, 90)) +
    ggalt::coord_proj(paste0("+proj=laea")) +
    # coord_polar(start = -180*pi/180) +
    theme_void() +
    scale_fill_divergent(midpoint = 0) +
    # scale_fill_divergent() +
    guides(fill = "none") +
    NULL
metR
ggsave("logo/cap2.svg")

