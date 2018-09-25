# nocov start

.tidy2matrix <- function(data, formula, value.var, fill = NULL, ...) {
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])
    data <- as.data.table(data)
    data[, row__ := .GRP, by = c(row.vars)]
    data[, col__ := .GRP, by = c(col.vars)]
    if (is.null(fill)){
        fill <- 0
        rowdims <- data[col__ == 1, (row.vars), with = FALSE]
        coldims <- data[row__ == 1, (col.vars), with = FALSE]
    } else {
        rowdims <- unique(data[, (row.vars), with = FALSE])
        coldims <- unique(data[, (col.vars), with = FALSE])
    }

    data.m <- matrix(fill[1], nrow = max(data[["row__"]]),
                     ncol = max(data[["col__"]]))
    data.m[cbind(data[["row__"]], data[["col__"]])] <- data[[value.var]]

    return(list(matrix = data.m,
                coldims = coldims,
                rowdims = rowdims))
}


seq_range <- function(x, by = ggplot2::resolution(x, zero = FALSE),...) {
    r <- range(x)
    seq.int(r[1], r[2], by = by, ...)
}

is.error <- function(x) inherits(x, "try-error")



# from data.table
guess <- function (x)
{
    if ("value" %chin% names(x))
        return("value")
    if ("(all)" %chin% names(x))
        return("(all)")
    var <- names(x)[ncol(x)]
    message("Using '", var, "' as value column. Use 'value.var' to override")
    return(var)
}

# from ggplot2
`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}
# from ggplot2
is.waive <- function (x) {
    inherits(x, "waiver")
}

element_render <- function(theme, element, ..., name = NULL) {

    # Get the element from the theme, calculating inheritance
    el <- ggplot2::calc_element(element, theme)
    if (is.null(el)) {
        message("Theme element ", element, " missing")
        return(ggplot2::zeroGrob())
    }

    grob <- element_grob(el, ...)
    ggname(paste(element, name, sep = "."), grob)
}

ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}


width_cm <- function(x) {
    if (is.grob(x)) {
        grid::convertWidth(grid::grobWidth(x), "cm", TRUE)
    } else if (is.unit(x)) {
        grid::convertWidth(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, width_cm, numeric(1))
    } else {
        stop("Unknown input")
    }
}
height_cm <- function(x) {
    if (is.grob(x)) {
        grid::convertHeight(grid::grobHeight(x), "cm", TRUE)
    } else if (is.unit(x)) {
        grid::convertHeight(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, height_cm, numeric(1))
    } else {
        stop("Unknown input")
    }
}

message_wrap <- function(...) {
    msg <- paste(..., collapse = "", sep = "")
    wrapped <- strwrap(msg, width = getOption("width") - 2)
    message(paste0(wrapped, collapse = "\n"))
}

.seq_range <- function(x, by = ggplot2::resolution(x, zero = FALSE),...) {
    r <- range(x)
    seq.int(r[1], r[2], by = by, ...)
}



#' @importFrom stats line runif var
#' @importFrom utils head
if(getRversion() >= "2.15.1") {
    utils::globalVariables(
        c("as", "dep.names", "ecdf", "equal", "fft", "hasArg", "id",
          "ind.names", "inside", "int.level", "land", "latrad", "lon", "lonrad",
          "piece", "psi", "psi.dx", "psi.dxx", "psi.dxy", "psi.dy", "psi.dyy",
          "r2", "sd", "setTxtProgressBar", "time", "txtProgressBar",
          "u.mean", "v.mean", "write.csv", "x", "y", "z", ".", "time2",
          "group", "step", "point", "change", "end", "level", "m", "rotate",
          "x.d", "y.d", "PC", "step2", "runif", "N", "angle", "var", "head",
          "col__", "row__"))
}


`%>%` <- dplyr::`%>%`

.is.reggrid <- function(data, coords) {
    lengths <- data[, .N, by = coords]$N
    !any(lengths > 1)
}


.simple.scale <- function(x) {
    r <- range(x)
    (x - r[1])/diff(r)
}

isFALSE <- function (x) {
        is.logical(x) && length(x) == 1L && !is.na(x) && !x
}


# nocov end

