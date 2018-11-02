library(metR)


context("season")
test_that("season assigns season", {
    expect_equal(as.character(season(1)), "DJF")
    expect_equal(as.character(season(1, lang = "es")),
                 "DEF")
    expect_equal(as.character(season(c(1, 3))),
                 c("DJF", "MAM"))
    expect_equal(levels(season(1)),
                 c("DJF", "MAM", "JJA", "SON"))
})


context("Derivate")
test_that("Derivative works", {
    expect_equal({
        x <- 1:10
        y <- 1:10
        Derivate(x ~ y, cyclical = FALSE)[[1]][2]
    }, 1)
    expect_equal({
        x <- 1:10
        y <- x^2
        Derivate(y ~ x, order = 2, cyclical = FALSE)[[1]][2]
    }, 2)
    expect_equal({
        data <- data.frame(x = 1:10, y = 1:10)
        Derivate(x ~ y, data = data, cyclical = FALSE)[[1]][2]
    }, 1)
    expect_equal({
        x <- 1:10
        y <- x^3
        Derivate(y ~ x, order = 3, cyclical = FALSE)[[1]][3]
    }, 6)
})

test_that("Derivative fills edges", {
    expect_equal({
        x <- 1:10
        y <- x^3
        is.na(Derivate(y ~ x, order = 3, cyclical = FALSE, fill = TRUE)[[1]][1])
    }, FALSE)
})

test_that("Derivative checks boundary conditions", {
    expect_error({
        x <- 1:10
        y <- 1:10
        z <- 1:10
        Derivate(x ~ y + z + x, cyclical = c(TRUE, FALSE))
    })
})

test_that("Derivative uses spherical coords", {
    expect_equal(Derivate(gh ~ lon + lat, data = geopotential[date == date[1]],
                          sphere = TRUE),
                 {
                     g <- Derivate(gh ~ lon + lat, data = geopotential[date == date[1]])
                     g[[1]] <- g[[1]]*180/pi/(6371000*cos(geopotential[date == date[1]]$lat*pi/180))
                     g[[2]] <- g[[2]]*180/pi/6371000
                     g
                 })
})

test_that("Divergence returns divergence", {
    expect_equal({
        grid <- expand.grid(x = 1:10, y = 1:10)
        grid$v <- rnorm(100)
        grid$u <- rnorm(100)
        Divergence(u + v ~ x + y, data = grid)
    },
    {
        d <- Derivate(u + v ~ x + y, data = grid)
        d$u.dx + d$v.dy})
})

test_that("Laplacian returns laplacian", {
    expect_equal({
        grid <- expand.grid(x = 1:10, y = 1:10)
        grid$v <- rnorm(100)
        grid$u <- rnorm(100)
        Laplacian(u + v ~ x + y, data = grid)$u.lap
    },
    {
        d <- Derivate(u + v ~ x + y, data = grid, order = 2)
        d$u.ddx + d$u.ddy})
})

test_that("Vorticity returns vorticidy", {
    expect_equal({
        grid <- expand.grid(x = 1:10, y = 1:10)
    grid$v <- rnorm(100)
    grid$u <- rnorm(100)
    Vorticity(u + v ~ x + y, data = grid)
    },
    {
        d <- Derivate(u + v ~ x + y, data = grid)
        d$v.dx - d$u.dy
    })
})


context("MaskLand")
test_that("Water is still water", {
    expect_equal(MaskLand(120, 14), FALSE)
    expect_equal(MaskLand(360-58, -34), TRUE)
    expect_equal(MaskLand(-58, -34, wrap = c(-180, 180)), TRUE)
})

test_that("MaskLand respects boundaries", {
    expect_equal(MaskLand(361, 15), NA)
    expect_equal(MaskLand(0, -91), NA)
    expect_equal(MaskLand(0, 91), NA)
    expect_equal(MaskLand(360-58, 90, wrap = c(-180, 180)), NA)
})

test_that("MaskLand respects order", {
    expect_equal(MaskLand(c(120, 360-58), c(14, -34)), c(FALSE, TRUE))
})
