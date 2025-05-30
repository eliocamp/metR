
test_that("GetTopography gets topography", {
    skip_if_offline()
    skip_if_not_installed("terra")
    expect_s3_class(GetTopography(280, 330, 0, -60, resolution = 0.5,
                                  file.dir = "~"), "data.frame")
    topo <- GetTopography(280, 330, 0, -60, resolution = 0.5)
    expect_known_value(topo, "topography")
    expect_message(GetTopography(280, 330, 0, -60, resolution = 0.5, verbose = TRUE),
                   "Fetching cached field.")
})

