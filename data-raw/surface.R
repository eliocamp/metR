## code to prepare `surface` dataset goes here

library(magrittr)
library(data.table)

surface <- fread("data-raw/surface.csv")

surface <- surface %>%
    .[south_north %% 4 == 0 &
            west_east %% 4 == 0] %>%
    .[, .(lon, lat, height = hgt)]

proj_string <- "+proj=lcc +lat_1=-30.9659996032715 +lat_2=-30.9659996032715 +lat_0=-30.9660034179688 +lon_0=-63.5670013427734 +a=6370000 +b=6370000"

surface[, c("x", "y") := proj4::project(list(lon, lat), proj_string, inverse = FALSE)] %>%
    .[, `:=`(x = round(x, -3),
             y = round(y, -4))]

usethis::use_data(surface, overwrite = TRUE)
