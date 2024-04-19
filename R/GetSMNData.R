#' Get Meteorological data
#'
#' Downloads minimum and maximum temperature station data from
#' Argentina's National Weather Service's public access. Data availability is not
#' guaranteed so you are encouraged to check it on
#' [the website](https://www.smn.gob.ar/descarga-de-datos).
#'
#' This function is defunct.
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
#' @export
GetSMNData <- function(date, type = c("hourly", "daily", "radiation"),  bar = FALSE,
                       cache = TRUE, file.dir = tempdir()) {
    .Defunct()

}
