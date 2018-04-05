#' Geopotential height
#'
#' Monthly geopotential field at 700hPa south of 20°S from January 1990 to
#' December 2000.
#'
#' @format
#' A data.table with 53224 rows and 5 variables.
#' \describe{
#'   \item{lon}{longitude in degrees}
#'   \item{lat}{latitude in degrees}
#'   \item{lev}{level in hPa}
#'   \item{gh}{geopotential height in meters}
#'   \item{date}{date}
#'   }
#' @source \url{https://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.derived.pressure.html}
"geopotential"
