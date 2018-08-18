
lon180 <- function(lon) {
    l <<- lon
    # lon <- lon %% 360
    trans <- lon > 180 & is.finite(lon)
    lon[trans] <- lon[trans] - 360
    lon
}

lon360 <- function(lon) {
    # lon <- (lon + 180) %% 360 - 180
    trans <- lon < 0 & is.finite(lon)
    lon[trans] <- lon[trans] + 360
    lon
}

lon180_trans <- function() {
    scales::trans_new("lon180",
                      lon180,
                      lon360)
}

lon360_trans <- function() {
    scales::trans_new("lon360",
                      lon360,
                      lon180)
}
