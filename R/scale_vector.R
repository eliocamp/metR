## Scale_magnitude?


scale_mag <- function(length = 0.1,
                         max = waiver(),
                         default_unit = "lines") {
    # if (!is.unit(length)) length <- ggplot2::unit(length, default_unit)

    continuous_scale("mag",
                     "mag",
                     identity,
                     rescaler = rescale_mag(length, max),
                     guide = "none")
}

# scale_type.mag <- function(x) "vector"

rescale_mag <- function(length, max) {
    function(x, from) {
        if (is.waive(max)) max <- max(x, na.rm = T)
        scales::rescale(x, c(0, length), c(0, max))
    }
}


