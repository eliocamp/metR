library(metR)

context("Small functions")

test_that("Percentile works", {
    expect_equal(Percentile(1:10),
                 seq(0.1, 1, by = 0.1))
})

test_that("Mag supports multiple dimensions", {
    expect_equal(Mag(3, 4, 5),
                 sqrt(3^2 + 4^2 + 5^2))
    expect_error(Mag(3, 4, c(5, 3)))
    expect_equal(Mag(c(1, 3), c(1, 4)),
                 sqrt(c(1, 3)^2 + c(1, 4)^2))
    expect_equal(Mag(as.list(1:5)),
                 Mag(1, 2, 3, 4, 5))
})

test_that("Similar things are similar", {
    expect_equal(c(3, 5) %~% 4.5,
                 c(FALSE, TRUE))
    expect_equal(Similar(c(3, 5), 4.5, tol = 0.1),
                 c(FALSE, FALSE))
})


test_that("JumpBy jumps", {
    expect_equal(JumpBy(1:10, 2),
                 c(1, 3, 5, 7, 9))
    expect_equal(JumpBy(1:10, 2, 2),
                 c(2, 4, 6, 8, 10))
    expect_equal(JumpBy(1:10, 2, fill = 1000),
                 c(1, 1000, 3, 1000, 5, 1000, 7, 1000, 9, 1000))
})



test_that(".is.regular_grid works", {
    x <- rnorm(100)
    y <- rnorm(100)

    irreg <- data.table::data.table(x, y)

    reg <- data.table::CJ(x = 1:10, y = (1:10)^2)

    expect_false(irreg[, .is.regular_grid(x, y)])
    expect_true(reg[, .is.regular_grid(x, y)])
    expect_false(reg[x*y < 600, .is.regular_grid(x,y)])
})


