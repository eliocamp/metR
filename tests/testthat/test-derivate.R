context("Derivate internals")

test_derv <- function(y, cyclical = FALSE, fill = FALSE, equispaced = TRUE) {
    y <- sort(y)
    x <- sin(y)
    dx <- cos(y)
    ddx <- -sin(y)

    dx_num <- .derv(x, y,  cyclical = cyclical, fill = fill,
                    equispaced = equispaced)
    nas <- 2*(!fill) * (!cyclical)

    expect_numeric(dx_num)
    expect_equal(sum(is.na(dx_num)), nas)
    expect_true(sd(dx_num - dx, na.rm = TRUE) < 0.0001)

    ddx_num <- .derv(x, y, 2, cyclical = cyclical, fill = fill,
                     equispaced = equispaced)
    nas <- nas*2

    expect_numeric(ddx_num)
    expect_equal(sum(is.na(ddx_num)), nas)

    expect_true(sd(ddx_num - ddx, na.rm = TRUE) < 0.01)


    ddx2_num <- .derv(dx_num, y, 1, cyclical = cyclical, fill = fill,
                      equispaced = equispaced)

    expect_equal(sum(ddx_num - ddx2_num, na.rm = TRUE), 0)
}




test_that("Works with equal grid", {
    y <- seq(0, 2*pi, length.out = 500)[-1]
    test_derv(y)
    test_derv(y, fill = TRUE)
    test_derv(y, cyclical = TRUE)
    expect_error(.derv(x, y, cyclical = TRUE, equispaced = FALSE))
})




test_that("Works with unequal grid", {
    y <- runif(500, 0, 2*pi)
    test_derv(y)
    test_derv(y, fill = TRUE)
    expect_error(.derv(x, y, cyclical = TRUE))
})


context("Derivate interface")
test_that("Derivative works", {
    expect_equal({
        x <- 1:10
        y <- 1:10
        Derivate(x ~ y, cyclical = FALSE)[[1]][2]
    }, 1)
    expect_equal({
        x <- 1:10
        y <- x^2
        Derivate(y ~ x, order = 2, fill = TRUE, cyclical = FALSE)[[1]][2]
    }, 2)
    expect_equal({
        data <- data.frame(x = 1:10, y = 1:10)
        Derivate(x ~ y, data = data, cyclical = FALSE)[[1]][2]
    }, 1)
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
