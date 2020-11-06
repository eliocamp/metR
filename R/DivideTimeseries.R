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
#' library(data.table)
#' gdata <- geopotential[lat == -30 & lon == 0]
#' g <- ggplot(gdata, aes(date, gh)) +
#'     geom_line() +
#'     geom_smooth() +
#'     scale_x_date(date_breaks = "1 year", date_labels = "%b")
#' DivideTimeseries(g, gdata$date, n = 2, "Date", "Max Temperature")
#' @family ggplot2 helpers
#' @export
# nocov start
DivideTimeseries <- function(g, x, n = 2, xlab = "x", ylab = "y") {
    warning("'DivideTimeseries' is deprecated, use ggwrap instead",
            ' (https://github.com/wilkox/ggwrap)')

    if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("DivideTimeseries needs the gridExtra package. Install it with `install.packages(\"gridExtra\")`")
    }

    M <- max(x)
    m <- min(x)
    step <- (M - m)/n
    g <- g + ggplot2::labs(x="", y="") +
        ggplot2::theme(axis.title = ggplot2::element_blank())

    plots <- list()

    for (i in 1:n) {
        if (i == 1) {
            pl <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(g))
            leg <- which(vapply(pl$grobs, function(x) x$name, "") == "guide-box")
            try(legend.new <- pl$grobs[[leg]])
        }
        pl <- g +
            ggplot2::coord_cartesian(xlim = as.Date(c(m + step*(i-1), m + i*step))) +
            ggplot2::theme(axis.title = ggplot2::element_blank()) +
            ggplot2::guides(color = FALSE)
        pl <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(pl))
        plots[[i]] <- pl
    }
    if (exists("legend.new")) {
        plots[[n + 1]] <- legend.new
    }
    gridExtra::grid.arrange(grobs = plots, ncol = 1, heights = c(rep(10, n), 2),
                            bottom = xlab, left = ylab)
}
# nocov end
