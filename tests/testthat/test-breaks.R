library(metR)


context("Breaks")

test_that("functions return functions", {
  expect_equal(is.function(MakeBreaks()), TRUE)
  expect_equal(is.function(AnchorBreaks()), TRUE)
})

test_that("Anchor is in breaks", {
  expect_equal(0 %in% AnchorBreaks(0, 1)(c(-10, 10 + 0.0005)), TRUE)
  expect_equal(
    0 %in% AnchorBreaks(0, 1.00001)(c(-10 + 0.00000001, 10 + 0.0005)),
    TRUE
  )
  expect_equal(0 %in% AnchorBreaks(0, 1, 0)(c(-10, 10 + 0.0005)), FALSE)
})


test_that("breaks work with binwith = NULL", {
  expect_equal(AnchorBreaks(1)(c(1, 5), 1), 0:6)
  expect_equal(MakeBreaks()(c(1, 5), 1), 1:5)
})


test_that("robust breaks work", {
  z <- metR:::with_seed(42, rnorm(100))
  df <- data.frame(z = c(50, -50, z))

  expect_equal(
    robust_breaks()(df, binwidth = 0.5),
    c(-Inf, seq(-3, 2.5, by = 0.5), Inf)
  )
})