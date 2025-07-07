library(rcdo)

dir.create("tests/testthat/era5/")


"data-raw/era5/*" |>
    Sys.glob() |>
    lapply(\(x) cdo_samplegrid(x, 8) |>
               cdo_execute(paste0("tests/testthat/era5/", basename(x)))
    )

