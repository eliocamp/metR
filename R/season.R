#' Assign seasons to months
#'
#' @param month A numeric vector of months or a vector of dates.
#' @param hemisphere A character with the hemisphere for which to assign the seasons.
#' @param lang Language to use.
#'
#' @return
#' A factor vector of the same length as \code{month} with the season of each
#' month.
#'
#' @examples
#' season(1, lang = "en", h = "n")
#' season(1, lang = "en")
#' season(as.Date("2017-01-01"))
#'
#' @aliases AssignSeason
#' @export
season <- function(month, hemisphere = c("south", "north"),
                         lang = c("en", "es")) {

    if (.is.somedate(month)) month <- lubridate::month(month)

    hemisphere <- substr(tolower(hemisphere[[1]]), 1, 1)
    if (lang[1] == "en") {
        sum <- "DJF" #"Summer"

    } else {
        sum <- "EJM" # "Verano"
        # win <- "Invierno"
        # aut <- "Oto\u00f1o"
        # spr <- "Primavera"
    }
    win <- "JJA" # "Winter"
    aut <-  "MAM"#  "Autumn"
    spr <- "SON" #"Spring"
    if (hemisphere == "s") {
        seasons <- c(sum, sum, rep(c(aut, win, spr), each = 3), sum)
    } else {
        seasons <- c(win, win, rep(c(spr, sum, aut), each = 3), win)
    }
    return(factor(seasons[month], levels = c(sum, aut, win, spr)))
}

#' @export
AssignSeason <- season


.is.somedate <- function(x) {
    inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")
}
