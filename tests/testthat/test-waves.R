

context("FitWave")

t <- seq(0, 2*pi, length.out = 100)[-1]
x <- 3*cos(t) + 2*cos(2*t)

test_that("fits waves", {
    fit <- FitWave(x, k = 1:3)

    expect_true(inherits(fit, "list"))
    expect_equal(fit$amplitude, c(3, 2, 0))
    expect_equal(fit$k, 1:3)

    expect_error(FitWave(x, -3))

    x[3] <- NA
    fit_na <- FitWave(x, 1:3)
    expect_true(inherits(fit_na, "list"))

    expect_equal(fit_na$amplitude, rep(NA_real_, 3))
    expect_equal(fit_na$k, 1:3)

})
