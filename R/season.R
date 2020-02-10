#' Assign seasons to months
#'
#' @param x A vector of dates (alternative a numeric vector of months, for `season()`)
#' @param lang Language to use.
#'
#' @return
#' `season()` returns a factor vector of the same length as `x` with the trimester of each
#' month.
#' `seasonaly()` returns a date vector of the same length as `x` with the date "rounded" up to the centre
#' month of each season.
#' `is.full_season()` returns a logical vector of the same length as `x` that is true only if the
#' 3 months of each season for each year (December counts for the following year) are present in the dataset.
#'
#' @examples
#' season(1, lang = "en")
#' season(as.Date("2017-01-01"))
#'
#' seasonaly(as.Date(c("2017-12-01", "2018-01-01", "2018-02-01")))
#'
#' is.full_season(as.Date(c("2017-12-01", "2018-01-01", "2018-02-01", "2018-03-01")))
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
#' @rdname season
seasonaly <- function(x) {
    checks <- makeAssertCollection()
    assertVector(x, add = checks)
    assert(checkDateish(x))
    reportAssertions(checks)
    if (is.character(x)) x <- as.Date(x)


    times <- unique(x)
    m <- data.table::month(times)
    times_org <- times
    lubridate::year(times[m == 12]) <- data.table::year(times[m == 12]) + 1
    s <- season(times)

    lubridate::month(times) <- (as.numeric(s) - 1)*3 + 1

    times[match(x, times_org)]
}


#' @export
#' @rdname season
is.full_season <- function(x) {
    checks <- makeAssertCollection()
    assertVector(x, add = checks)
    assert(checkDateish(x))
    reportAssertions(checks)
    if (is.character(x)) x <- as.Date(x)

    times <- unique(x)

    times_org <- times
    m <- data.table::month(times)
    lubridate::year(times[m == 12]) <- data.table::year(times[m == 12]) + 1
    season <- season(times)
    year <- data.table::year(times)
    n <- NULL
    complete <- data.table::data.table(times, year, season)[
        , n := .N, by = .(year, season)][n == 3][
        , n := NULL]

    x %in% times_org[times %in% complete$times]
}


#' @export
AssignSeason <- season


.is.somedate <- function(x) {
    inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")
}
