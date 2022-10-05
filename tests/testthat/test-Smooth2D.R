
volcano <- reshape2::melt(datasets::volcano, value.name = "original")
volcano$noisy <- with(volcano, original + 1.5*rnorm(length(original)))

test_that("Smooth2D returns a vector", {
     expect_vector(with(volcano, Smooth2D(Var2, Var1, noisy, method = smooth_svd(0.005))), size = nrow(volcano))
     expect_vector(with(volcano, Smooth2D(Var2, Var1, noisy, method = smooth_dct(kx = 0.4))), size = nrow(volcano))
})

test_that("Smooth2D returns the correct order", {
    expect_equal(with(volcano, Smooth2D(Var2, Var1, noisy, method = smooth_svd(0))),
                 volcano$noisy)

    expect_equal(with(volcano, Smooth2D(Var2, Var1, noisy, method =  smooth_dct(kx = 1))),
                 volcano$noisy)
})
