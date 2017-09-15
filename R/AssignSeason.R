#' Assign seasons to months
#'
#' @param month A numeric vector of months or a vector of dates.
#' @param hemisphere A character with the hemisphere for wich to assign the seaons.
#' @param lang Language to use.
#'
#' @return
#' A factor vector of the same length as \code{month} with the season of each
#' month.
#'
#' @examples
#' season()
#' @aliases AssignSeason
#' @export
#' @import lubridate
season <- function(month, date, hemisphere = c("south", "north"),
                         lang = c("es", "en")) {

    if (is.Date(month)) month <- lubridate::month(month)

    hemisphere <- substr(tolower(hemisphere[[1]]), 1, 1)
    if (lang[1] == "en") {
        sum <- "Summer"
        win <- "Winter"
        aut <- "Autumn"
        spr <- "Spring"
    } else {
        sum <- "Verano"
        win <- "Invierno"
        aut <- "Oto\u00f1o"
        spr <- "Primavera"
    }
    if (hemisphere == "s") {
        seasons <- c(sum, sum, rep(c(aut, win, spr), each = 3), sum)
    } else {
        seasons <- c(win, win, rep(c(spr, sum, aut), each = 3), win)
    }
    return(factor(seasons[month], levels = c(sum, aut, win, spr)))
}

#' @export
AssignSeason <- season
