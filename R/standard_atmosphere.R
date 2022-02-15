#' Standard atmosphere
#'
#' Utilities to use the International Standard Atmosphere. It uses the
#' International Standard Atmosphere up to the tropopause (11 km by definition)
#' and then extends up to the 500 km using the ARDC Model Atmosphere.
#' @name standard_atmosphere
NULL

#' @param height height in meter
#' @param pressure pressure in pascals
#' @param height_in,pressure_in units of height and pressure, respectively.
#' Possible values are "km", "m" for height and "hPa" and "Pa" for pressure.
#' Alternatively, it can be a numeric constant that multiplied to convert the
#' unit to meters and Pascals respectively. (E.g. if height is in feet,
#' use `height_in = 0.3048`.)
#' @param name,breaks,labels,guide arguments passed to [ggplot2::sec_axis()]
#' @param n desiderd number of breaks.
#' @param ... extra arguments passed to [scales::breaks_extended].
#'
#' @details
#' `sa_pressure()`, `sa_height()`, `sa_temperature()` return, respectively,
#' pressure (in pascals), height (in meters) and temperature (in Kelvin).
#'
#' `sa_height_trans()` and `sa_pressure_trans()` are two transformation functions
#' to be used as the `trans` argument in ggplot2 scales (e.g. `scale_y_continuous(trans = "sa_height"`).
#'
#' `sa_height_axis()`  and `sa_pressure_axis()` return a secondary axis that transforms to
#' height or pressure respectively to be used as ggplot2 secondary axis
#' (e.g. `scale_y_continuous(sec.axis = sa_height_axis())`).
#'
#' For convenience, and unlike the "primitive" functions, both the transformation
#' functions and the axis functions input and output in hectopascals and kilometres
#' by default.
#'
#' @examples
#' height <- seq(0, 100*1000, by = 1*200)
#'
#' # Temperature profile that defines the standard atmosphere (in degrees Celsius)
#' plot(sa_temperature(height) - 273.15, height, type = "l")
#'
#' # Pressure profile
#' plot(sa_pressure(height), height, type = "l")
#'
#' # Use with ggplot2
#' library(ggplot2)
#' data <- data.frame(height = height/1000,               # height in kilometers
#'                    pressure = sa_pressure(height)/100) # pressures in hectopascals
#'
#' # With the sa_*_axis functions, you can label the approximate height
#' # when using isobaric coordinates#'
#' ggplot(data, aes(height, pressure)) +
#'   geom_path() +
#'   scale_y_continuous(sec.axis = sa_height_axis("height"))
#'
#' # Or the approximate pressure when using physical height
#' ggplot(data, aes(pressure, height)) +
#'   geom_path() +
#'   scale_y_continuous(sec.axis = sa_pressure_axis("level"))
#'
#' # When working with isobaric coordinates,using a linear scale exagerates
#' # the thickness of the lower levels
#' ggplot(temperature[lat == 0], aes(lon, lev)) +
#'    geom_contour_fill(aes(z = air)) +
#'    scale_y_reverse()
#'
#' # Using the standard atmospehre height transormation, the result
#' # is an approximate linear scale in height
#' ggplot(temperature[lat == 0], aes(lon, lev)) +
#'    geom_contour_fill(aes(z = air)) +
#'    scale_y_continuous(trans = "sa_height", expand = c(0, 0))
#'
#' # The result is very similar to using a reverse log transform, which is the
#' # current behaviour of scale_y_level(). This transformation slightly
#' # overextends the higher levels.
#' ggplot(temperature[lat == 0], aes(lon, lev)) +
#'    geom_contour_fill(aes(z = air)) +
#'    scale_y_level()
#'
#' @references Standard atmosphere—Glossary of Meteorology. (n.d.).
#' Retrieved 22 February 2021, from \url{https://glossary.ametsoc.org/wiki/Standard_atmosphere}
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
sa_height_trans <- function(pressure_in = "hPa", height_in = "km") {

    mult_pressure <- get_multiplicative_constant(tolower(pressure_in),
                                                 hpa = 100,
                                                 pa = 1)

    mult_height <- get_multiplicative_constant(tolower(height_in),
                                               km = 1000,
                                               m = 1)

    scales::trans_new("height_sa",
                      transform = function(x) {
                          standard_atmosphere$altitude(x*mult_pressure)/mult_height
                      },
                      inverse =  function(x) {
                          standard_atmosphere$pressure(x*mult_height)/mult_pressure
                      },
                      breaks = scales::log_breaks(n = 6),
                      format = scales::number_format(drop0trailing = TRUE,
                                                     big.mark = "",
                                                     trim = TRUE),
                      domain = sort(standard_atmosphere$pressure(c(-610, 500000)/mult_height)/mult_pressure))
}

#' @export
#' @rdname standard_atmosphere
sa_pressure_trans <- function(height_in = "km", pressure_in = "hPa") {

    mult_pressure <- get_multiplicative_constant(tolower(pressure_in),
                                                 hpa = 100, pa = 1)

    mult_height <- get_multiplicative_constant(tolower(height_in),
                                               km = 1000,
                                               m = 1)
    scales::trans_new("pressure_sa",
                      transform =  function(x) {
                          standard_atmosphere$pressure(x*mult_height)/mult_pressure
                      },
                      inverse =  function(x) {
                          standard_atmosphere$altitude(x*mult_pressure)/mult_height
                      },
                      breaks = scales::log_breaks(n = 6),
                      format = scales::number_format(drop0trailing = TRUE,
                                                     big.mark = "",
                                                     trim = FALSE),
                      domain = c(-610, 500000)/mult_height)
}


get_multiplicative_constant <- function(x, ... ) {
    if (is.numeric(x)) {
        return(x)
    }
    switch(tolower(x),
           ...)
}

#' @export
#' @rdname standard_atmosphere
sa_height_breaks <- function(n = 6, pressure_in  = "hPa",
                             height_in = "km", ...) {
    force(n)
    mult_pressure <- get_multiplicative_constant(tolower(pressure_in),
                                                 hpa = 100,
                                                 pa = 1)
    mult_height <- get_multiplicative_constant(tolower(height_in),
                                               km = 1000,
                                               m = 1)
    function(x) {
        breaks <- scales::breaks_extended(n = n, ...)(standard_atmosphere$pressure(x*mult_height)/mult_pressure)
        breaks <- standard_atmosphere$altitude(breaks*mult_pressure)/mult_height

        breaks <- breaks[is.finite(breaks)]
        breaks <- breaks[breaks > x[1] & breaks < x[2]]

        breaks <- sort(breaks)

        diff <- diff(breaks)
        diff <- c(diff[1], diff)

        precision <- -(floor(log10(diff)))


        breaks <- vapply(seq_along(breaks), function(i) round(breaks[i], precision[i]), numeric(1))


        unique(breaks)
    }

}

#' @export
#' @rdname standard_atmosphere
sa_height_axis <- function(name = ggplot2::waiver(),
                           breaks = sa_height_breaks(pressure_in  = pressure_in,
                                                     height_in = height_in),
                           labels = ggplot2::waiver(),
                           guide = ggplot2::waiver(),
                           pressure_in = "hPa", height_in = "km") {

    mult_pressure <- get_multiplicative_constant(tolower(pressure_in),
                                                 hpa = 100,
                                                 pa = 1)

    mult_height <- get_multiplicative_constant(tolower(height_in),
                                               km = 1000,
                                               m = 1)


    ggplot2::sec_axis(trans = ~ standard_atmosphere$altitude(.x*mult_pressure)/mult_height,
                      name = name,
                      breaks = breaks,
                      labels = labels,
                      guide = guide)
}

#' @export
#' @rdname standard_atmosphere
sa_pressure_axis <- function(name = ggplot2::waiver(),
                             breaks = scales::log_breaks(n = 6),
                             labels = scales::number_format(drop0trailing = TRUE,
                                                            big.mark = "",
                                                            trim = FALSE),
                             guide = ggplot2::waiver(),
                             height_in = "km", pressure_in = "hPa") {
    mult_pressure <- get_multiplicative_constant(pressure_in,
                                                 hpa = 100,
                                                 pa = 1)

    mult_height <- get_multiplicative_constant(height_in,
                                               km = 1000,
                                               m = 1)

    fun <- function(x) standard_atmosphere$pressure(x*mult_height)/mult_pressure
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


