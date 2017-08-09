#' Quasi stationary waves
#'
#'
#' @param y numeric vector to transform
#' @param k numeric vector of wave numbers to keep or wave numbers to compute
#' @param x numeric vector of locations (in radians)
#' @param amplitude numeric vector of amplitudes
#' @param phase numeric vector of phases
#'
#' @return
#' \code{FitQsWave} reurns a named list of amplitude, phase and
#' wave number. \cr
#'
#' \code{BuildQsField} returns a vector.
#'
#' @details
#' \code{FitQsWave} uses \code{\link{fft}} to make a fourier transform of the
#' data and then returns a list of parameters for each wave number kept.
#' The  amplitude (A), phase (\eqn{\phi}) and wave number (k) satisfy:
#' \deqn{y = \sum A cos((x - \phi)k)}
#' The phase is calculated so that it lies between 0 and \eqn{2\pi/k} so it
#' represents the location (in radians) of the first maximum of each wave number.  \cr
#'
#' \code{BuildQsField} is, in a way, the opositve. It computes  the
#' the above mentioned sum but for only the selected wave numbers.
#'
#' @rdname QsWave
#' @family meteorology functions
#' @aliases BuildQsWave FitQsWave
#' @export
FitQsWave <- function(y, k = 1) {
    # Calcula los parámetros de números de onda. Se lleva bien con n vector
    # y data.table.
    # Entra:
    #   x: vector de entrada
    #   n: vector con los números de onda a calcular
    # Sale:
    #   una lista con la amplitud, la fase, la varianza explicada y el número
    #   de onda.
    f <- fft(y)
    f <- f/length(f)
    amp <- Mod(f)*2
    phase <- -Arg(f)

    # Hago que la fase esté entre 0 y 2/k*pi
    phase[phase < 0] <- phase[phase < 0] + 2*pi
    phase <- phase/(seq_along(phase) - 1)

    r <- amp^2/(2*sd(y)^2)
    k <- k + 1

    cols <- c("amplitude", "phase", "r2", "k")
    ret <- list(amp[k], phase[k], r[k], k - 1)
    names(ret) <- cols
    return(ret)
}


#' @rdname QsWave
#' @export
BuildQsField <- function(x, amplitude, phase, k) {
    field <- vector(length = length(x))
    for (i in k) {
        field <- field + amplitude[i]*cos((x - phase[i])*k[i])
    }
    return(field)
}
