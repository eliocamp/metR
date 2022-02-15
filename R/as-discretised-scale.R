#' Create discretised versions of continuous scales
#'
#'
#' @param scale_function a scale function (e.g. `scale_fill_divergent`)
#'
#'
#' @return
#' A function with the same arguments as `scale_function` that works with discretised
#' values.
#'
#' @examples
#' library(ggplot2)
#' scale_fill_brewer_discretised <- as.discretised_scale(scale_fill_distiller)
#'
#' @seealso scale_fill_discretised
#'
#' @export
#' @rdname discretised_scale
as.discretised_scale <- function(scale_function) {
    new_fun <- function(...) {
        call <- match.call()

        if (is.null(call$super)) {
            call$super <- ScaleDiscretised
        }

        if (is.null(call$guide)) {
            call$guide <- ggplot2::guide_colorsteps()
        }

        call[[1]] <- scale_function

        eval(call)
    }

    formals(new_fun) <- formals(scale_function)
    new_fun
}
