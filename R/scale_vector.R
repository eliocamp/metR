#' Scale for vector magnitudes
#'
#' Allows to control the size of the arrows in [geom_arrow].
#' Highly experimental.
#'
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(seals, aes(long, lat)) +
#'     geom_vector(aes(dx = delta_long, dy = delta_lat), skip = 2)
#'
#' g + scale_mag("Seals velocity")
#'
#' g + scale_mag("Seals velocity", limits = c(0, 1))
#'
#' @export
scale_mag <- function(name = ggplot2::waiver(),
                      n.breaks = 1,
                      breaks = ggplot2::waiver(),
                      oob = no_censor,
                      ...) {

    if (is.waive(breaks) & n.breaks == 1) {
        breaks <- function(range) return(max(scales::breaks_extended(4)(range)))
    }


    ggplot2::continuous_scale(aesthetics = "mag",
                              palette = scales::identity_pal(),
                              name = name,
                              oob = oob,
                              breaks = breaks,
                              n.breaks	= n.breaks,
                              ...
    )
}

#' @export
#' @rdname scale_mag
scale_mag_continuous <- scale_mag

no_censor <- function(x, range) {
    x
}
