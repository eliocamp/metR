#' Surface temperature
#'
#' Surface temperature timeseries from 2000-01-01 to 2012-12-10 for argentinian
#' stations in the La Plata Basin. While \code{claris} stores the actual data,
#' \code{claris.stations} stores the station metadata.
#'
#' @format
#' Claris: A data.table with 179933 rows and 4 variables.
#' \describe{
#'   \item{id}{station id}
#'   \item{date}{date}
#'   \item{max}{maximum daily temperature}
#'   \item{min}{minimum daily temperature}
#' }
#' Claris.stations: A data.table with 51 rows and 6 variables.
#' \describe{
#'   \item{id}{station id}
#'   \item{name}{station name}
#'   \item{wmo_id}{station id in the World Meteorological Organization database}
#'   \item{elevation}{station elevation in meters}
#'   \item{lat}{latitude in degrees}
#'   \item{lon}{longitude in degrees from 0 to 360}
#' }
#' @name claris
#' @source Penalba, O. C., Rivera, J. A. and Pántano, V. C. (2014), The CLARIS LPB database: constructing a long-term daily hydro-meteorological dataset for La Plata Basin, Southern South America. Geoscience Data Journal. doi: 10.1002/gdj3.7
"claris"


#' @rdname claris
"claris.stations"
