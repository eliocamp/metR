dates <- as.Date(c("2022-09-05", "2022-09-06"))
test_that("hourly works", {
    skip_if_offline()
    skip_on_cran()

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


    results <- GetSMNData(dates, type = "radiation")

    expect_true(inherits(results, "data.frame"))
    names <- c("date", "global", "diffuse", "station")
    expect_equal(colnames(results), names)
    expect_true(nrow(results) > 0)
    expect_equal(lubridate::tz(results$date), "UTC")
})
