context("Fitlm")


n <- 100
x <- rnorm(n)
y <- rnorm(n)
z <- rnorm(n)
good_fit <- lm(x ~ y + z)


test_that("regression works", {
    sim_fit <- coef(good_fit)
    sim_fit <- list(term = names(sim_fit), estimate = unname(sim_fit))

    expect_equal(FitLm(x, y, z), sim_fit)

    summ <- summary(good_fit)

    sim_fit <- c(sim_fit,
                 list(std.error = unname(summ$coefficients[, 2]),
                      df = rep(summ$df[2], 3),
                      r.squared = rep(summ$r.squared, 3),
                      adj.r.squared = rep(summ$adj.r.squared, 3)))

    expect_equal(FitLm(x, y, z, se = TRUE), sim_fit)
})


test_that("special cases work", {
    expect_equal(FitLm(x, rnorm(n), z = rnorm(n))$term[-1], c("V1", "z"))

    expect_equal(FitLm(rnorm(1), rnorm(1))$estimate, c(NA_real_, NA_real_))

    x[-1] <- NA
    expect_equal(FitLm(x, y)$estimate, c(NA_real_, NA_real_))
})


test_that("weighted, regression works", {
    weights <- runif(n, 0.1, 1)
    good_fit <- lm(x ~ y + z, weights = weights)

    sim_fit <- coef(good_fit)
    sim_fit <- list(term = names(sim_fit), estimate = unname(sim_fit))

    expect_equal(FitLm(x, y, z, weights = weights), sim_fit)

    summ <- summary(good_fit)

    sim_fit <- c(sim_fit,
                 list(std.error = unname(summ$coefficients[, 2]),
                      df = rep(summ$df[2], 3),
                      r.squared = rep(summ$r.squared, 3),
                      adj.r.squared = rep(summ$adj.r.squared, 3)))

    expect_equal(FitLm(x, y, z, se = TRUE, weights = weights), sim_fit)
})



test_that("Supports NAs", {
    y[10] <- NA
    good_fit <- lm(x ~ y + z)

    sim_fit <- coef(good_fit)
    sim_fit <- list(term = names(sim_fit), estimate = unname(sim_fit))

    expect_equal(FitLm(x, y, z), sim_fit)

    summ <- summary(good_fit)

    sim_fit <- c(sim_fit,
                 list(std.error = unname(summ$coefficients[, 2]),
                      df = rep(summ$df[2], 3),
                      r.squared = rep(summ$r.squared, 3),
                      adj.r.squared = rep(summ$adj.r.squared, 3)))

    expect_equal(FitLm(x, y, z, se = TRUE), sim_fit)
})
