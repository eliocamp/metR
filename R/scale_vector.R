## Scale_magnitude?

#' @export
scale_mag <- function(length = 0.1,
                         max = waiver(),
                         default_unit = "lines", guide = "vector") {
    # if (!is.unit(length)) length <- ggplot2::unit(length, default_unit)

    continuous_scale("mag",
                     "mag",
                     identity,
                     breaks = vector_breaks(length, max),
                     oob = no_censor,
                     rescaler = rescale_mag(length, max),
                     guide = guide
                     )
}

# scale_type.mag <- function(x) "vector"

vector_breaks <- function(length, max) {
    function(range) {
        if (is.waive(max)) return(range[2])
        return(max)
    }
}
no_censor <- function(x, range) {
    x
}


rescale_mag <- function(length, max) {
    function(x, from) {
        if (is.waive(max)) max <- max(x, na.rm = TRUE)
        scales::rescale(x, c(0, length), c(0, max))
    }
}


