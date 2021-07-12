#' Scale for vector magnitudes
#'
#' Allows to control the size of the arrows in [geom_arrow].
#' Highly experimental.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param max_size size of the arrow in centimetres
#' @param max magnitude of the reference arrow in data units. Will be the
#' maximum value if `waiver()`
#' @param guide type of legend
#' @param default_unit ignored
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(seals, aes(long, lat)) +
#'     geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)
#'
#' g + scale_mag("Seals velocity")
#'
#' g + scale_mag("Seals velocity", max = 1)
#'
#' g + scale_mag("Seals velocity", max_size = 2)
#' g + scale_mag("Seals velocity", default_unit = "mm")
#' @export
scale_mag <- function(name = ggplot2::waiver(),
                      # breaks = waiver(),
                      labels = ggplot2::waiver(),
                      # limits = NULL,
                      max_size = 1,
                      default_unit = "cm",
                      max = ggplot2::waiver(),
                      guide =  guide_vector(),
                      ...) {
    # if (!grid::is.unit(length)) length <- ggplot2::unit(length, default_unit)
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
    # guide = guide_vector(default.unit = "cm")

    ggplot2::continuous_scale("mag",
                              "mag",
                              identity,
                              name = name,
                              breaks = breaks,
                              labels = labels,
                              limits = NULL,
                              rescaler = rescale_mag(max_size, max),
                              oob = no_censor,
                              guide = guide,
                              ...
    )
}

#' @export
#' @rdname scale_mag
scale_mag_continuous <- scale_mag

# #' @export
# #' @rdname scale_mag
# scale_dx_continuous <- scale_mag

# #' @export
# scale_dy_continuous <- scale_mag

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


