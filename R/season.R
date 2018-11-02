#' Assign seasons to months
#'
#' @param x A numeric vector of months or a vector of dates
#' @param lang Language to use.
#'
#' @return
#' A factor vector of the same length as \code{x} with the trimestre of each
#' month.
#'
#' @examples
#' season(1, lang = "en")
#' season(as.Date("2017-01-01"))
#'
#' @aliases AssignSeason
#' @export
season <- function(x, lang = c("en", "es")) {
    checks <- makeAssertCollection()
    assertChoice(lang[1], c("en", "es"), .var.name = "lang", add = checks)
    assertVector(x, any.missing = FALSE, add = checks)
    assert(
        checkIntegerish(x, lower = 1, upper = 12),
        checkDateish(x),
        .var.name = "month")

    reportAssertions(checks)

    if (is.character(x)) x <- as.Date(x)
    if (.is.somedate(x)) x <- lubridate::month(x)

    if (lang[1] == "en") {
        djf <- "DJF" #"Summer"

    } else {
        djf <- "DEF" # "Verano"
        # win <- "Invierno"
        # aut <- "Oto\u00f1o"
        # spr <- "Primavera"
    }
    jja <- "JJA" # "Winter"
    mam <-  "MAM"#  "Autumn"
    son <- "SON" #"Spring"spr

    seasons <- c(djf, djf, rep(c(mam, jja, son), each = 3), djf)
    return(factor(seasons[x], levels = c(djf, mam, jja, son)))
}

#' @export
AssignSeason <- season


.is.somedate <- function(x) {
    inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")
}
