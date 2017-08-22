library(meteoR)

context("")
test_that("AssignSeason assigns season", {
    expect_equal(as.character(AssignSeason(1)), "Verano")
    expect_equal(as.character(AssignSeason(1, hemisphere = "n")),
                 "Invierno")
    expect_equal(as.character(AssignSeason(1, lang = "en")),
                 "Summer")
    expect_equal(as.character(AssignSeason(c(1, 3))),
                 c("Verano", "Otoño"))
    expect_equal(levels(AssignSeason(1)),
                 c("Verano", "Otoño", "Invierno", "Primavera"))
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


