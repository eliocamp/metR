#' Get Meteorological data
#'
#' Downloads station data from Argentina's National Meteorological Service's
#' public access. Data availability is restricted to one year prior.
#'
#' @examples
#' \dontrun{
#' dates <- seq.Date(as.Date("2016-07-14"), as.Date("2017-07-14"), by = "1 day")
#' data <- GetData(dates, bar = FALSE)    # long!
#'
#' library(ggplot2)
#' ggplot(subset(data, station == "BASE BELGRANO II"),
#'        aes(date, (t.max + t.min)/2)) +
#'     geom_line()
#' }
#' @export
#' @import RCurl
GetData <- function(date, source = "SMN", bar = FALSE) {
    return.data <- data.frame()
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
            return.data <- rbindlist(list(return.data, data))
        }
    }
    if (bar == TRUE) close(pb)
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



