## Scale_magnitude?

#' @export
scale_mag <- function(name = waiver(),
                      # breaks = waiver(),
                      labels = waiver(),
                      # limits = NULL,
                      max_size = 1,
                      default_unit = "cm",
                      max = waiver(),
                      guide = guide_vector(default.unit = default_unit),
                      ...) {
    # if (!is.unit(length)) length <- ggplot2::unit(length, default_unit)
    # if (is.waive(breaks)) {
        if (is.waive(max)) {
            breaks <- function(range) return(range[2])
        } else {
            breaks <- function(range) return(max)
        }
    # }

    # default_unit <- formals(guide)$default_unit
    # max_size <- units::con
    # guide <- guide_vector(default )

    ggplot2::continuous_scale("mag",
                     "mag",
                     identity,
                     name = name,
                     breaks = waiver(),
                     labels = labels,
                     limits = NULL,
                     rescaler = rescale_mag(max_size, max),
                     oob = no_censor,
                     guide = guide,
                     ...
                     )
}

# scale_type.mag <- function(x) "vector"

vector_breaks <- function(max_size, max) {
    function(range) {
        if (is.waive(max)) return(range[2])
        return(max)
    }
}


no_censor <- function(x, range) {
    x
}


rescale_mag <- function(max_size, max) {
    function(x, from) {
        if (is.waive(max)) max <- max(x, na.rm = TRUE)
        scales::rescale(x, c(0, max_size), c(0, max))
    }
}


