#' Smooths a 2D field
#'
#' Smooth a 2D field using a user-supplied method.
#'
#' @param x,y Vector of x and y coordinates
#' @param value Vector of values
#' @param method The method to use smooth. Must be a function that takes a matrix
#' and returns the smoothed matrix. Build-in methods are `smooth_svd()` and `smooth_dct()`.
#'
#' @return
#' A vector of the same length as value.
#'
#' @details
#' `smooth_svd()` computes the SVD of the field and reconstructs it keeping only
#' the leading values that ensures a maximum variance lost.
#' `smooth_dct()` computes the Discrete Cosine Transform of the field and sets
#' a proportion of the components to zero.
#'
#' @examples
#' library(ggplot2)
#' # Creates a noisy version of the volcano dataset and applies the smooth
#' volcano <- reshape2::melt(datasets::volcano, value.name = "original")
#' volcano$noisy <- with(volcano, original + 1.5*rnorm(length(original)))
#'
#' volcano$smooth_svd <- with(volcano, Smooth2D(Var2, Var1, noisy, method = smooth_svd(0.005)))
#' volcano$smooth_dct <- with(volcano, Smooth2D(Var2, Var1, noisy, method = smooth_dct(kx = 0.4)))
#'
#' volcano <- reshape2::melt(volcano, id.vars = c("Var1", "Var2"))
#'
#' ggplot(volcano, aes(Var1, Var2)) +
#'   geom_contour(aes(z = value, color = ..level..)) +
#'   scale_color_viridis_c() +
#'   coord_equal() +
#'   facet_wrap(~variable, ncol = 2)
#' @export
Smooth2D <- function (x, y, value, method = smooth_svd(0.01)) {
    o <- order(y, x)
    # Transform into a matrix
    matrix <- matrix(value[o], nrow = length(unique(x)),  byrow = FALSE)

    method <- match.fun(method)
    matrix <- method(matrix)

    c(matrix)[order(o)]
}

#' @param kx,ky Proportion of components to keep in the x and
#' y direction respectively. Lower values increase the smoothness.
#' @rdname Smooth2D
#' @export
smooth_dct <- function(kx = 0.5, ky = kx) {
    force(kx)
    force(ky)
    check_packages("gsignal", "smooth_dct")
    function(matrix) {
        # Compute the Discreete Cosine Transform
        # We could also use fft, but it suffers from edge effects
        dct <- gsignal::dct2(matrix)

        # Define the components to keep
        # Remmeber that the FFT is symmetrical
        kx <- c(0, seq_len(floor(nrow(dct)/2 * kx)))
        kx <- c(kx + 1, nrow(dct) - kx[kx != 0] + 1)

        ky <- c(0, seq_len(floor(ncol(dct)/2 * ky)))
        ky <- c(ky + 1, ncol(dct) - ky[ky != 0] + 1)

        # Replace with zeros and invert
        dct[, -ky] <- 0
        dct[-kx, ] <- 0
        gsignal::idct2(dct)
    }

}

#' @param  variance_lost Maximum percentage of variance lost after smoothing.
#' @rdname Smooth2D
#' @export
smooth_svd <- function(variance_lost = 0.01) {
    force(variance_lost)
    function(matrix) {
        if (isTRUE(all.equal(variance_lost, 0L))) {
            return(matrix)
        }
        m <- mean(matrix)
        matrix <- matrix - m
        svd <- svd(matrix)
        total_variance <- norm(abs(matrix), type = "F")

        variance_accumulated <- cumsum(svd$d^2/total_variance^2)
        variance_kept <- 1 - variance_lost

        keep <- seq_len(which(variance_accumulated - variance_kept > 0)[1])

        smooth <- svd$u[, keep] %*% diag(svd$d[keep], nrow = length(keep)) %*% t(svd$v[, keep])
        smooth <- smooth + m
        smooth
    }
}
