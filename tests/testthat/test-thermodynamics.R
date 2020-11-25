test_that("IdealGas", {
    p <- 1013*100
    t <- 20 + 273.15
    rho <- IdealGas(p, t)

    expect_equal(round(rho, 6), 1.203788)
    expect_equal(IdealGas(p, rho = rho), t)
    expect_equal(IdealGas(t = t, rho = rho), p)


})


test_that("Adiabat", {
    p <- 1013*100
    t <- 20 + 273.15
    theta <- Adiabat(p, t)

    expect_equal(round(theta, 4), 292.0702)
    expect_equal(Adiabat(p, theta = theta), t)
    expect_equal(Adiabat(t = t, theta = theta), p)
})


test_that("VirtualTemperature", {
    p <- 1013*100
    t <- 20 + 273.15
    tv <- t + 5
    e <- VirtualTemperature(p, t, tv = tv)

    expect_equal(round(e, 3), 4494.205)
    expect_equal(VirtualTemperature(p, e = e, tv = tv), t)
    expect_equal(VirtualTemperature(p, e = e, t = t), tv)
    expect_equal(VirtualTemperature(t = t, e = e, tv = tv), p)
})


test_that("MixingRatio", {
    p <- 1013*100
    w <- 0.02887632
    e <- MixingRatio(p, w = w)

    expect_equal(round(e, 3), 4494.204)
    expect_equal(MixingRatio(e = e, w = w), p)
    expect_equal(MixingRatio(p = p, e = e), w)

})

test_that("ClausiusClapeyron", {
    t <- 273
    es <- ClausiusClapeyron(t)

    expect_equal(round(es, 4), 604.3262)
    expect_equal(ClausiusClapeyron(es = es), t)

})


test_that("DewPoint", {
    p <- 1013*100
    td <- 25 +273.15
    ws <- DewPoint(p, td = td)

    expect_equal(round(ws, 8), 0.02003907)
    expect_equal(round(DewPoint(p, ws), 2), td)
    expect_equal(DewPoint(ws = ws, td = td), p)
})
