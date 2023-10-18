library(testthat)
library(metR)
# library(vdiffr)

on_cran <- !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
if (on_cran) data.table::setDTthreads(2)

test_check("metR")
