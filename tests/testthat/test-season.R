
context("season functions")

levels <-  c("DJF", "MAM", "JJA", "SON")
niveles <- c("DEF", "MAM", "JJA", "SON")

test_that("season works", {

    expect_factor(season(1, lang = "en"))
    expect_equal(season(1, lang = "en"), factor("DJF", levels = levels))
    expect_equal(season(1, lang = "es"), factor("DEF", levels = niveles))

    expect_equal(season(1), season("2019-01-01"))
    expect_equal(season(1), season("2019-01-25"))

    expect_equal(season(c(1, 3, 6, 9)), factor(levels, levels = levels))
})


test_that("seasonally works", {
    expect_equal(seasonally(c("2017-12-01", "2018-01-31", "2018-02-01")),
                 as.Date(c("2018-01-15", "2018-01-15", "2018-01-15")))



})

test_that("full season works", {
    expect_equal(
        is.full_season(as.Date(c("2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01"))),
        c(TRUE, TRUE, TRUE, FALSE))
})
