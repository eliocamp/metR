#' Divides long timeseries for better reading
#'
#' Long timeseries can be compressed to the point of being unreadable when
#' plotted on a page. This function takes a ggplot object of a timeseries and
#' divides it into panels so that the time dimension gets stretched for better
#' readability.
#'
#' @param g ggplot object
#' @param x The vector that was used in g for the x axis (must be of class Date)
#' @param n Number of panels
#' @param xlab x axis label
#' @param ylab y axis label
#'
#' @return
#' Draws a plot.
#'
#' @examples
#' library(ggplot2)
#' series <- expand.grid(year = 1980:2000,
#'                      month = 1:12)
#' series$var = rnorm(nrow(series))
#' series$type = sample(letters[1:3], nrow(series), replace = TRUE)
#' series$date = with(series, as.Date(paste0(year, "-", month, "-01")))
#'
#' g <- ggplot(series, aes(date, var, color = type)) +
#'     geom_line()
#' DivideTimeseries(g, n = 3, series$date, xlab = "Date", ylab = "Variable")
#' @family ggplo2 helpers
#' @export
DivideTimeseries <- function(g, x, n = 2, xlab = "x", ylab = "y") {
    M <- max(x)
    m <- min(x)
    step <- (M - m)/n
    g <- g + labs(x="", y="") + theme(axis.title = element_blank())
    library(grid)
    library(gridExtra)
    plots <- list()

    for (i in 1:n) {
        if (i == 1) {
            pl <- ggplot_gtable(ggplot_build(g))
            leg <- which(sapply(pl$grobs, function(x) x$name) == "guide-box")
            try(legend.new <- pl$grobs[[leg]])
        }
        pl <- g + coord_cartesian(xlim = as.Date(c(m + step*(i-1), m + i*step))) +
            theme(axis.title = element_blank()) + guides(color = FALSE)
        pl <- ggplot_gtable(ggplot_build(pl))
        plots[[i]] <- pl
    }
    if (exists("legend.new")) {
        plots[[n + 1]] <- legend.new
    }
    grid.arrange(grobs = plots, ncol = 1, heights = c(rep(10, n), 2), bottom = xlab, left = ylab)
}

