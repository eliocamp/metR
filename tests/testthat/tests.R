library(meteoR)

context("")
test_that("AssignSeason assigns season", {
    expect_equal(as.character(AssignSeason(1)), "Verano")
    expect_equal(as.character(AssignSeason(1, hemisphere = "n")),
                 "Invierno")
    expect_equal(as.character(AssignSeason(1, lang = "en")),
                 "Summer")
    expect_equal(as.character(AssignSeason(c(1, 3))),
                 c("Verano", "Oto\u00f1o"))
    expect_equal(levels(AssignSeason(1)),
                 c("Verano", "Oto\u00f1o", "Invierno", "Primavera"))
})




test_that("Derivative works", {
    expect_equal({
        x <- 1:10
        y <- 1:10
        Derivate(x ~ y, bc = "none")[2]},
        1)
    expect_equal({
        data <- data.frame(x = 1:10, y = 1:10)
        Derivate(x ~ y, bc = "none")[2]},
        1)
    })

context("MakeMask")
test_that("Water is still water", {
    expect_equal(MakeMask(120, 14), FALSE)
    expect_equal(MakeMask(360-58, -34), TRUE)
    expect_equal(MakeMask(-58, -34, wrap = c(-180, 180)), TRUE)
})

test_that("MakeMask respects boundaries", {
    expect_equal(MakeMask(361, 15), NA)
    expect_warning(MakeMask(361, 15))
    expect_equal(MakeMask(0, -91), NA)
    expect_equal(MakeMask(0, 91), NA)
    expect_equal(MakeMask(360-58, 90, wrap = c(-180, 180)), NA)
})
