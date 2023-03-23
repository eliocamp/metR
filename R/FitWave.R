#' Fourier transform
#'
#' Perform a fourier transform of the data and return the
#'
#' @param y numeric vector to transform
#' @param k numeric vector of wave numbers
#' @param x numeric vector of locations (in radians)
#' @param amplitude numeric vector of amplitudes
#' @param phase numeric vector of phases
#' @param wave optional list output from `FitWave`
#' @param sum whether to perform the sum or not (see Details)
#' @param action integer to disambiguate action for k = 0 (see Details)
#'
#' @return
#' `FitWaves` returns a a named list with components
#' \describe{
#'   \item{k}{wavenumbers}
#'   \item{amplitude}{amplitude of each wavenumber}
#'   \item{phase}{phase of each wavenumber in radians}
#'   \item{r2}{explained variance of each wavenumber}
#' }
#'
#' `BuildWave` returns a vector of the same length of x with the reconstructed
#' vector if `sum` is `TRUE` or, instead, a list with components
#' \describe{
#'   \item{k}{wavenumbers}
#'   \item{x}{the vector of locations}
#'   \item{y}{the reconstructed signal of each wavenumber}
#' }
#'
#' `FilterWave` returns a vector of the same length as `y`
#' `
#' @details
#' `FitWave` uses [fft] to make a fourier transform of the
#' data and then returns a list of parameters for each wave number kept.
#' The  amplitude (A), phase (\eqn{\phi}) and wave number (k) satisfy:
#' \deqn{y = \sum A cos((x - \phi)k)}
#' The phase is calculated so that it lies between 0 and \eqn{2\pi/k} so it
#' represents the location (in radians) of the first maximum of each wave number.
#' For the case of k = 0 (the mean), phase is arbitrarily set to 0.
#'
#' `BuildWave` is `FitWave`'s inverse. It reconstructs the original data for
#' selected wavenumbers. If `sum` is `TRUE` (the default) it performs the above
#' mentioned sum and returns a single vector. If is `FALSE`, then it returns a list
#' of k vectors consisting of the reconstructed signal of each wavenumber.
#'
#' `FilterWave` filters or removes wavenumbers specified in `k`. If `k` is positive,
#' then the result is the reconstructed signal of `y` only for wavenumbers
#' specified in `k`, if it's negative, is the signal of `y` minus the wavenumbers
#' specified in `k`. The argument `action` must be be manually set to `-1` or `+1`
#' if `k=0`.
#'
#' `WaveEnvelope` computes the wave envelope of `y` following Zimin (2003). To compute
#' the envelope of only a restricted band, first filter it with `FilterWave`.
#'
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#'
#' data(geopotential)
#' library(data.table)
#' # January mean of geopotential height
#' jan <- geopotential[month(date) == 1, .(gh = mean(gh)), by = .(lon, lat)]
#'
#' # Stationary waves for each latitude
#' jan.waves <- jan[, FitWave(gh, 1:4), by = .(lat)]
#' library(ggplot2)
#' ggplot(jan.waves, aes(lat, amplitude, color = factor(k))) +
#'     geom_line()
#'
#' # Build field of wavenumber 1
#' jan[, gh.1 := BuildWave(lon*pi/180, wave = FitWave(gh, 1)), by = .(lat)]
#' ggplot(jan, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.1, color = after_stat(level))) +
#'     coord_polar()
#'
#' # Build fields of wavenumber 1 and 2
#' waves <- jan[, BuildWave(lon*pi/180, wave = FitWave(gh, 1:2), sum = FALSE), by = .(lat)]
#' waves[, lon := x*180/pi]
#' ggplot(waves, aes(lon, lat)) +
#'     geom_contour(aes(z = y, color = after_stat(level))) +
#'     facet_wrap(~k) +
#'     coord_polar()
#'
#' # Field with waves 0 to 2 filtered
#' jan[, gh.no12 := gh - BuildWave(lon*pi/180, wave = FitWave(gh, 0:2)), by = .(lat)]
#' ggplot(jan, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.no12, color = after_stat(level))) +
#'     coord_polar()
#'
#' # Much faster
#' jan[, gh.no12 := FilterWave(gh, -2:0), by = .(lat)]
#' ggplot(jan, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.no12, color = after_stat(level))) +
#'     coord_polar()
#'
#' # Using positive numbers returns the field
#' jan[, gh.only12 := FilterWave(gh, 2:1), by = .(lat)]
#' ggplot(jan, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.only12, color = after_stat(level))) +
#'     coord_polar()
#'
#' # Compute the envelope of the geopotential
#' jan[, envelope := WaveEnvelope(gh.no12), by = .(lat)]
#' ggplot(jan[lat == -60], aes(lon, gh.no12)) +
#'     geom_line() +
#'     geom_line(aes(y = envelope), color = "red")
#'
#' @references Zimin, A.V., I. Szunyogh, D.J. Patil, B.R. Hunt, and E. Ott, 2003: Extracting Envelopes of Rossby Wave Packets. Mon. Wea. Rev., 131, 1011–1017, \doi{10.1175/1520-0493(2003)131<1011:EEORWP>2.0.CO;2}
#' @name waves
#' @family meteorology functions
#' @aliases BuildWave FitWave
#' @export
FitWave <- function(y, k = 1) {
    assertIntegerish(k, lower = 0, any.missing = FALSE)

    if (any(is.na(y))) {
        nas <- rep(NA_real_, length(k))
        return(list(amplitude = nas,
                    phase = nas,
                    k = k,
                    r2 = nas))
    }

    f <- fft(y)
    l <- length(f)
    f <- (f/l)[1:ceiling(l/2)]
    amp <- Mod(f)
    amp[-1] <- amp[-1]*2
    # amp[1] <- mean(y)
    phase <- -Arg(f)

    # Hago que la fase esté entre 0 y 2/k*pi
    phase[phase < 0] <- phase[phase < 0] + 2*pi
    phase <- phase/(seq_along(phase) - 1)
    phase[1] <- 0

    r <- amp^2/sum(amp[-1]^2)
    r[1] <- 0
    k <- k + 1

    ret <- list(amp[k], phase[k], k - 1, r[k])
    names(ret) <- c("amplitude", "phase", "k", "r2")
    return(ret)
}


#' @rdname waves
#' @export
BuildWave <- function(x, amplitude, phase, k,
                       wave = list(amplitude = amplitude, phase = phase, k = k),
                       sum = TRUE) {
    assertListSameLength(wave, names = TRUE)
    assertFlag(sum)

    if (sum == TRUE) {
        y <- lapply(seq_along(wave$k),
                    function(i) wave$amplitude[i]*cos((x - wave$phase[i])*wave$k[i]))
        y <- Reduce("+", y)
        return(y)
    } else {
        field <- data.table::setDT(expand.grid(x = x, k = wave$k))
        field <- field[wave, on = "k"]
        field[, y := amplitude*cos((x - phase)*k), by = k]
        return(as.list(field[, .(k, x, y)]))
    }
}

#' @rdname waves
#' @export
FilterWave <- function(y, k, action = sign(k[k != 0][1])) {
    assertIntegerish(k, any.missing = FALSE)

    assertNumeric(y)

    if (any(is.na(y))) {
        return(rep(NA_real_, length(y)))
    }

    f <- fft(y)
    # Need to remove the k+1 spots (because index 1 is k = 0)
    # and the N - k + 1 because of symmetry.
    k1 <- abs(k)
    if (is.na(action)) action <- 1
    k1 <- c(k1 + 1, length(y) - k1[k1 != 0] + 1)
    index <- -action*k1
    f[index] <- 0 + 0i
    Re(fft(f, inverse = T))/length(y)
}



#' @rdname waves
#' @export
WaveEnvelope <- function(y) {
    assertNumeric(y)

    if (any(is.na(y))) {
        return(rep(NA_real_, length(y)))
    }


    N <- length(y)
    x_hat <- fft(y)/N
    k <- 1:ceiling(N/2)
    x_hat[k] <- 0
    Mod(fft(x_hat, inverse = T))*2
}
