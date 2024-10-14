#' Get Meteorological data
#' This function is defunct.
#'
#' @param date date vector of dates to fetch data
#' @param type type of data to retrieve
#' @param bar logical object indicating whether to show a progress bar
#' @param cache logical indicating if the results should be saved on disk
#' @param file.dir optional directory where to save and/or retrieve data
#'
#' @return
#' Nothing
#'
#' @export
GetSMNData <- function(date, type = c("hourly", "daily", "radiation"),  bar = FALSE,
                       cache = TRUE, file.dir = tempdir()) {
    .Defunct()

}
