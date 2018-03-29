#' Get Meteorological data
#'
#' Downloads minimum and maximum temperature station data from
#' Argentina's National Weather Service's
#' public access. Data availability is restricted to one year prior.
#'
#' @param date date vector of dates to fetch data
#' @param bar logical object indicating whether to show a progress bar
#'
#' @return
#' a dataframe with daily minimum and maximum temperature for all available stations
#' and each date.
#'
#' @examples
#' \dontrun{
#' dates <- seq.Date(today() - 30, today(), by = "1 day")
#' data <- SMNData(dates, bar = FALSE)    # long!
#'
#' library(ggplot2)
#' ggplot(subset(data, station == "BASE BELGRANO II"),
#'        aes(date, (t.max + t.min)/2)) +
#'     geom_line()
#' }
#'
#' @source https://ssl.smn.gob.ar/dpd/pron5d-calendario.php
#' @export
#' @import RCurl
SMNData <- function(date, bar = FALSE) {
    return.data <- data.frame()
    no_data <- vector()

    if (bar == TRUE) pb <- txtProgressBar(min = 1, max = length(date), style = 3)

    for (i in seq_along(date)) {
        if (bar == TRUE) setTxtProgressBar(pb, i)

        base.url <- "https://ssl.smn.gob.ar/dpd/descarga_pron5d.php?file="
        file.url <- paste0("obs", gsub("-", "", date[i]), ".txt")
        file.url <- RCurl::base64Encode(file.url)[1]
        file.url <- RCurl::base64Encode(file.url)[1]
        url <- paste0(base.url, file.url)

        obs <- readLines(url, warn = F)

        if (obs[1] != "El archivo no existe.") {
            data <- .CleanSMN(obs)
            data$date <- as.Date(date[i])
            return.data <- rbind(return.data, data)
        } else {
            no_data <- c(no_data, i)
        }
    }

    if (bar == TRUE) close(pb)

    if (length(return.data) == 0) stop("No data available for any of selected dates")
    if (length(no_data) != 0) {
        warning(paste0("No data for available for these dates: "), date[no_data])
    }
    return(return.data)
}


.CleanSMN <- function(obs) {
    obs <- obs[4:length(obs)]
    t.max <- as.numeric(stringr::str_sub(obs, 10, 15))
    t.min <- as.numeric(stringr::str_sub(obs, 16, 21))
    station <- as.character(stringr::str_sub(obs, 22))
    station <- stringr::str_trim(station)
    return(data.frame(t.max, t.min, station, stringsAsFactors = F))
}
