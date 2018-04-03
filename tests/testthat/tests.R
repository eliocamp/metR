library(metR)

context("")
test_that("season assigns season", {
    expect_equal(as.character(season(1)), "Verano")
    expect_equal(as.character(season(1, hemisphere = "n")),
                 "Invierno")
    expect_equal(as.character(season(1, lang = "en")),
                 "Summer")
    expect_equal(as.character(season(c(1, 3))),
                 c("Verano", "Oto\u00f1o"))
    expect_equal(levels(season(1)),
                 c("Verano", "Oto\u00f1o", "Invierno", "Primavera"))
})



context("Derivate")
test_that("Derivative works", {
    expect_equal({
        x <- 1:10
        y <- 1:10
        Derivate(x ~ y, cyclical = FALSE)[[1]][2]
    }, 1)
    expect_equal({
        data <- data.frame(x = 1:10, y = 1:10)
        Derivate(x ~ y, data = data, cyclical = FALSE)[[1]][2]
    }, 1)
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
