library(data.table)
data(geopotential)
geopotential <- geopotential[date == date[1]]
# new grid
x.out <- seq(0, 360, by = 10)
y.out <- seq(-90, 0, by = 10)

# Interpolate values to a new grid

test_that("interpolate works" , {
    expect_snapshot(geopotential[, Interpolate(gh ~ lon + lat, x.out, y.out)])

    x.out <- seq(0, 360, length.out = 10)
    y.out <- seq(-90, 0, length.out = 10)
    expect_snapshot(geopotential[, Interpolate(gh ~ lon + lat, x.out, y.out, grid = FALSE)])

    expect_snapshot(geopotential[, Interpolate(gh ~ lon + lat, x.out, y.out, grid = FALSE)])

    geopotential[, c("u", "v") := GeostrophicWind(gh, lon, lat)]
    expect_snapshot(geopotential[, Interpolate(u | v ~ lon + lat, x.out, y.out)])

    lats <- c(-34, -54, -30)   # start and end latitudes
    lons <- c(302, 290, 180)   # start and end longituded
    expect_snapshot(geopotential[, Interpolate(gh ~ lon + lat, as.path(lons, lats))])

})


