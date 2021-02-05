
data("geopotential")

geopotential[, c("u", "v") := GeostrophicWind(gh, lon, lat), by = date]
geopotential[, `:=`(dlon = dlon(u, lat),
                    dlat = dlat(v))]

test_that("trajectory works", {
    expect_known_value(Trajectory(dlon + dlat ~ lon + lat + date, x0 = 180, y0 = -60,
                                 data = na.omit(geopotential), res = 3,
                                 cyclical = c(TRUE, FALSE)), "trajectory1")

    expect_error(
    Trajectory(dlon  ~ lon + lat + date, x0 = 180, y0 = -60,
                                 data = na.omit(geopotential)),
    "LHS of formula must be of the form dx + dy", fixed = TRUE)


    expect_error(
        Trajectory(dlon + dlat ~ lat + date, x0 = 180, y0 = -60),
        "RHS of formula must be of the form x + y + t", fixed = TRUE)

})
