#' Standard atmosphere
#'
#' Utilities to use the International Standard Atmosphere. It uses the
#' International Standard Atmosphere up to the tropopause (11 km by definition)
#' and then extends up to the 500 km using the ARDC Model Atmosphere.
#'
#' @param height height in meter
#' @param pressure pressure in pascals
#' @param name,breaks,labels,guide arguments passed to [ggplot2::sec_axis()]
#'
#'
#' @details
#' `sa_pressure()`, `sa_height()`, `sa_temperature()` return, respectively,
#' pressure (in pascals), height (in meters) and temperature (in Kelvin).
#'
#' `height_sa_trans()` and `pressure_sa_trans()` are two transformation functions
#' to be used as the `trans` argument in ggplot2 scales (e.g. `scale_y_continuous(trans = "height_sa"`).
#'
#' `axis_height()`  and `axis_pressure()` return a secondary axis that transforms to
#' height or pressure respectively to be used as ggplot2 secondary axis
#' (e.g. `scale_y_continuous(sec.axis = axis_height())`).
#'
#' For convenience, and unlike the "primitive" functions, both the transformation
#' functions and the axis functions input and output in hectopascals and kilometers.
#'
#'
#' @examples
#' height <- seq(0, 100*1000, by = 1*1000)
#'
#' # Temperature profile that defines the standard atmosphere (in degree Celsius)
#' plot(sa_temperature(height) - 273.15, height, type = "l")
#'
#' # Pressure profile
#' plot(sa_pressure(height), height, type = "l", log = "x")
#'
#' # Use with ggplot2
#' library(ggplot2)
#' data <- data.frame(height = height, pressure = sa_pressure(height))
#'
#' ggplot(data, aes(pressure, height)) +
#'   geom_path() +
#'   scale_y_continuous(sec.axis = axis_pressure())
#'
#' # Doesn't work for now
#' ggplot(data, aes(pressure/100, height)) +
#'   geom_path() +
#'   scale_y_continuous(trans = "pressure_sa")
#'
#'
#' @references Standard atmosphere—Glossary of Meteorology. (n.d.). Retrieved 22 February 2021, from \url{https://glossary.ametsoc.org/wiki/Standard_atmosphere}
#' @export
#' @rdname standard_atmosphere
sa_pressure <- function(height) standard_atmosphere$pressure(height)

#' @export
#' @rdname standard_atmosphere
sa_height <- function(pressure) standard_atmosphere$pressure(pressure)

#' @export
#' @rdname standard_atmosphere
sa_temperature <- function(height) standard_atmosphere$temperature(height)


#' @export
#' @rdname standard_atmosphere
height_sa_trans <- function() {
    scales::trans_new("height_sa",
                      transform = function(x) {
                          standard_atmosphere$altitude(x*100)
                          },
                      inverse =  function(x) {
                          standard_atmosphere$pressure(x)/100
                          },
                      domain = sort(standard_atmosphere$pressure(c(-610, 500000)))/100)
}

#' @export
#' @rdname standard_atmosphere
pressure_sa_trans <- function() {
    scales::trans_new("pressure_sa",
                      transform =  function(x) standard_atmosphere$pressure(x)/100,
                      inverse =   function(x) standard_atmosphere$altitude(x*100),
                      domain = c(-610, 500000))
}

#' @export
#' @rdname standard_atmosphere
axis_height <- function(name = ggplot2::waiver(),
                        breaks = scales::pretty_breaks(),
                        labels = ggplot2::waiver(),
                        guide = ggplot2::waiver()) {
    ggplot2::sec_axis(trans = ~ standard_atmosphere$altitude(.x*100),
                      name = name,
                      breaks = breaks,
                      labels = labels,
                      guide = guide)
}

#' @export
#' @rdname standard_atmosphere
axis_pressure <- function(name = ggplot2::waiver(),
                          breaks = scales::pretty_breaks(),
                          labels = ggplot2::waiver(),
                          guide = ggplot2::waiver()) {
    fun <- function(x) standard_atmosphere$pressure(x)/100
    ggplot2::sec_axis(trans = fun,
                      name = name,
                      breaks = breaks,
                      labels = labels,
                      guide = guide)
}




atmosphere <- function(h0, P0, T0, lapse_rate) {
    force(h0)
    force(P0)
    force(T0)

    g <- 9.8665  # m/s^2
    R <- 287.04
    if (lapse_rate == 0) lapse_rate <- 1e-10
    # if (lapse_rate == 0) {
    #   list(
    #     pressure = function(h) {
    #       P0*exp(-g/(R*T0)*(h - h0))
    #     },
    #     altitude = function(p) {
    #       h0 + log(p/P0)*R*T0/g
    #     },
    #     temperature = function(h) {
    #       rep(T0, length(h))
    #     }
    #   )
    # } else {
    lapse_rate <- -lapse_rate
    list(
        pressure = function(h) {
            P0*(1 - lapse_rate*(h - h0)/T0)^(g/(R*lapse_rate))
        },
        altitude = function(p) {
            h0 + T0/lapse_rate*(1 - (p/P0)^(R*lapse_rate/g))
        },
        temperature = function(h) {
            T0 - lapse_rate*(h - h0)

        }
    )
    # }
}


stepwise <- function(breaks, functions) {
    force(breaks)
    force(functions)

    function(x) {
        if (is.null(x)) return(NULL)
        y <- rep(NA, length(x))
        for (i in seq_len(length(breaks) - 1)) {
            layer <- which(x >= breaks[i] & x <= breaks[i + 1])
            y[layer] <- functions[[i]](x[layer])
        }
        return(y)
    }
}


layers <- list(troposphere   = list(h0 = -610, T0 = 19 + 273.15, P0 = 108900,
                                    lapse_rate = -6.5/1000, hf = 11000),
               tropopause    = list(lapse_rate = 0, hf = 25000),
               stratosphere  = list(lapse_rate = 3/1000, hf = 47000),
               stratopause   = list(lapse_rate = 0, hf = 53000),
               mesosphere    = list(lapse_rate = -3.9/1000, hf = 75000),
               mesopause     = list(lapse_rate = 0, hf = 90000),
               thermosphere1 = list(lapse_rate = 3.5/1000, hf = 126000),
               thermosphere2 = list(lapse_rate = 10/1000, hf = 175000),
               thermosphere3 = list(lapse_rate = 5.8/1000, hf = 500000))


make_atmosphere <- function(layers) {
    layer_fun <- list()

    for (i in seq_along(layers)) {
        layer_fun[[i]] <- with(layers[[i]], atmosphere(h0 = h0, P0 = P0, T0 = T0, lapse_rate = lapse_rate))
        layers[[i]]$P0 <- layer_fun[[i]]$pressure(layers[[i]]$h0)
        layers[[i]]$Pf <- layer_fun[[i]]$pressure(layers[[i]]$hf)

        if (i + 1 <= length(layers)) {
            layers[[i + 1]]$h0 <- layers[[i]]$hf
            layers[[i + 1]]$P0 <- layer_fun[[i]]$pressure(layers[[i]]$hf)
            layers[[i + 1]]$T0 <- layer_fun[[i]]$temperature(layers[[i]]$hf)
        }
    }
    heights <- c(layers[[1]][["h0"]],
                 vapply(layers, function(x) x[["hf"]], numeric(1)))

    pressures <- c(layers[[1]][["P0"]],
                   vapply(layers, function(x) x[["Pf"]], numeric(1)))


    names(layer_fun) <- names(layers)


    pressure <- stepwise(heights, lapply(layer_fun, function(x) x$pressure))

    altitude <- stepwise(rev(pressures), rev(lapply(layer_fun, function(x) x$altitude)))

    temperature <- stepwise(heights, lapply(layer_fun, function(x) x$temperature))


    list(pressure = pressure,
         altitude = altitude,
         temperature = temperature)
}


standard_atmosphere <- make_atmosphere(layers)


