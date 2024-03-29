#' Get Meteorological data
#'
#' Downloads minimum and maximum temperature station data from
#' Argentina's National Weather Service's public access. Data availability is not
#' guaranteed so you are encouraged to check it on
#' [the website](https://www.smn.gob.ar/descarga-de-datos).
#'
#' @param date date vector of dates to fetch data
#' @param type type of data to retrieve
#' @param bar logical object indicating whether to show a progress bar
#' @param cache logical indicating if the results should be saved on disk
#' @param file.dir optional directory where to save and/or retrieve data
#'
#' @return
#' For `type = "hourly"`, a data.frame with observations of
#' \describe{
#'    \item{date}{date}
#'    \item{t}{temperature in degrees celsius}
#'    \item{rh}{relative humidity in %}
#'    \item{slp}{sea level pressure in hPa}
#'    \item{dir}{wind direction in clockwise degrees from 6 o'clock}
#'    \item{V}{wind magnitude in m/s}
#'    \item{station}{station name}
#' }
#'
#' For `type = "daily"`, a data.frame with observations of
#' \describe{
#'    \item{date}{date}
#'    \item{tmax}{maximum daily temperature in degrees celsius}
#'    \item{tmin}{minimum daily temperature in degrees celsius}
#'    \item{station}{station name}
#' }
#'
#' For `type = "radiation"`, a data.frame with observations of
#' \describe{
#'    \item{date}{date}
#'    \item{global}{global radiation in W/m^2}
#'    \item{diffuse}{diffuse radiation in W/m^2}
#'    \item{station}{station name}
#' }
#'
#'
#' @examples
#' \dontrun{
#' dates <- seq.Date(lubridate::today() - 30, lubridate::today(), by = "1 day")
#' data <- GetSMNData(dates, type = "daily", bar = TRUE)
#'
#' library(ggplot2)
#' ggplot(subset(data, station == "BASE BELGRANO II"),
#'        aes(date, (tmax + tmin)/2)) +
#'     geom_line()
#' }
#'
#' @source https://ssl.smn.gob.ar/dpd/pron5d-calendario.php
#' @export
GetSMNData <- function(date, type = c("hourly", "daily", "radiation"),  bar = FALSE,
                       cache = TRUE, file.dir = tempdir()) {
    checks <- makeAssertCollection()
    assertDate(as.Date(date), upper = as.Date(Sys.Date()),
               .var.name = "date", add = checks)
    assertChoice(type, c("hourly", "daily", "radiation"), add = checks)
    assertFlag(bar, add = checks)
    date <- as.Date(date)

    if (isTRUE(cache)) {
        assertAccess(file.dir, add = checks)
    }
    reportAssertions(checks)

    if (cache) {
        file.name <- paste0(digest::digest(list(date, type[1])), ".csv")
        file <- file.path(file.dir, file.name)
        if (file.exists(file)) {
            data <- data.table::fread(file)
            return(data)
        }
    }

    no_data <- vector()

    if (bar == TRUE) pb <- txtProgressBar(min = 0, max = length(date), style = 3)

    return.data <- data.table::rbindlist(lapply(seq_along(date), function(i) {
        if (bar == TRUE) setTxtProgressBar(pb, i)
        if (type[1] == "hourly") {
            data <- .smnhourly(date[i])
        } else if (type[1] == "daily") {
            data <- .smnobs(date[i])
        } else if (type[1] == "radiation") {
            data <- .smnrad(date[i])
        }
        if (is.null(data)) no_data <- i
        return(data)
    }))

    if (bar == TRUE) close(pb)

    if (length(return.data) == 0) stopf("No data available for any of selected dates.")
    if (length(no_data) != 0) {
        dates_no_data <- paste0(date[no_data], collapse = ", ")
        warningf("No data for available for these dates: %s.", dates_no_data)
    }

    if (cache) {
        data.table::fwrite(return.data, file = file)
    }
    return(return.data)
}

.smnhourly <- function(date) {
    file <- paste0("observaciones/datohorario",
                   stringr::str_remove_all(as.character(date), "-"), ".txt")
    url <- paste0("https://ssl.smn.gob.ar/dpd/descarga_opendata.php?file=", file)
    obs <- readLines(url, warn = FALSE)
    if (obs[1] != paste0(file, "El archivo no existe.")) {
        obs <- obs[3:length(obs)]
        variables <- c("start", "date", "hour", "t", "rh", "slp", "dir", "V", "station")
        charend <- c(1, 9, 15, 21, 26, 34, 39, 44, 101)

        obs <- data.table::as.data.table(lapply(seq_along(variables)[-1], function(x) {
            s <- stringr::str_squish(stringr::str_sub(obs, charend[x - 1], charend[x] -1))
            if(variables[x] != "station") s <- as.numeric(s)
            s
        }))

        data.table::setnames(obs, variables[-1])
        obs <- obs[, -1]
        obs <- obs[!is.na(hour)]
        obs$date <- with(obs, as.POSIXct(paste0(date, " ", hour, ":00:00"), tz = "America/Argentina/Buenos_Aires"))
        hour <- NULL
        obs[, hour := NULL]
        obs[, date := lubridate::with_tz(date, "UTC")][]
        return(obs)
    } else {
        return(NULL)
    }
}

.smnobs <- function(date) {
    file <- paste0("observaciones/obs",
                   stringr::str_remove_all(as.character(as.Date(date)), "-"), ".txt")
    url <- paste0("https://ssl.smn.gob.ar/dpd/descarga_opendata.php?file=", file)
    obs <- readLines(url, warn = FALSE)

    if (obs[1] != paste0(file, "El archivo no existe.")) {
        obs <- obs[4:length(obs)]
        variables <- c("start", "date", "tmax", "tmin", "station")
        charend <- c(1, 9, 15, 21, 101)

        obs <- data.table::as.data.table(lapply(seq_along(variables)[-1], function(x) {
            s <- stringr::str_squish(stringr::str_sub(obs, charend[x - 1], charend[x] -1))
            if(variables[x] != "station") s <- as.numeric(s)
            s
        }))

        data.table::setnames(obs, variables[-1])
        obs <- obs[, -1]
        obs <- obs[!is.na(date)]

        obs$date <- date
        return(obs)
    } else {
        return(NULL)
    }
}

.smnrad <- function(date) {
    file <- paste0("radiacionsolar/radsolar",
                   stringr::str_remove_all(as.character(as.Date(date)), "-"), ".txt")
    url <- paste0("https://ssl.smn.gob.ar/dpd/descarga_opendata.php?file=", file)
    obs <- data.table::fread(url, showProgress = FALSE)

    if (nrow(obs) == 0) return(NULL)

    bs <- obs[, 1:3]
    data.table::setnames(bs, c("date", "global", "diffuse"))
    bs$station <- "BsAs"

    ush <- obs[, c(1, 4:5)]
    data.table::setnames(ush, c("date", "global", "diffuse"))
    ush$station <- "Ushuaia"

    obs <- rbind(bs, ush)
    obs[, date := lubridate::as_datetime(date, tz = "America/Argentina/Buenos_Aires")]
    obs[, date := lubridate::with_tz(date, "UTC")]
    return(obs)
}
