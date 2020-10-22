library(ggplot2)

geo <- geopotential[date == date[1]][, c("u", "v") := GeostrophicWind(gh, lon, lat)]

test_that("waveflux returns w.x and w.y", {
    return <- with(geo, WaveFlux(gh, u, v, lon, lat, lev = 700))
    expect_known_output(return, "waveflux-return")
})

