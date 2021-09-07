
test_that("hourly works", {
    skip_if_offline()
    skip_on_cran()
    dates <- seq.Date(lubridate::today() - 3, lubridate::today() - 2, by = "1 day")
    results <- GetSMNData(dates, type = "hourly")

    expect_true(inherits(results, "data.frame"))
    names <- c("t", "rh", "slp", "dir", "V", "station", "date")
    expect_equal(colnames(results), names)
    expect_true(nrow(results) > 0)
    expect_equal(lubridate::tz(results$date), "UTC")
})

test_that("daily works", {
    skip_if_offline()
    skip_on_cran()
    dates <- seq.Date(lubridate::today() - 15, lubridate::today() - 2, by = "1 day")
    results <- GetSMNData(dates, type = "daily")

    expect_true(inherits(results, "data.frame"))
    names <- c("tmax", "tmin", "station", "date")
    expect_equal(colnames(results), names)
    expect_true(nrow(results) > 0)
    expect_equal(lubridate::tz(results$date), "UTC")
})


test_that("radiation works", {
    skip_if_offline()
    skip_on_cran()
    # skip("radiation data doesn't seem to be available.")
    dates <- seq.Date(lubridate::today() - 50, lubridate::today() - 45, by = "1 day")
    results <- GetSMNData(dates, type = "radiation")

    expect_true(inherits(results, "data.frame"))
    names <- c("date", "global", "diffuse", "station")
    expect_equal(colnames(results), names)
    expect_true(nrow(results) > 0)
    expect_equal(lubridate::tz(results$date), "UTC")
})
