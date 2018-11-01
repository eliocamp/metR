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



# Interleave (or zip) multiple units into one vector
interleave <- function(...) UseMethod("interleave")
#' @export
interleave.unit <- function(...) {
    do.call("unit.c", do.call("interleave.default", plyr::llply(list(...), as.list)))
}
#' @export
interleave.default <- function(...) {
    vectors <- list(...)

    # Check lengths
    lengths <- unique(setdiff(plyr::laply(vectors, length), 1))
    if (length(lengths) == 0) lengths <- 1
    stopifnot(length(lengths) <= 1)

    # Replicate elements of length one up to correct length
    singletons <- plyr::laply(vectors, length) == 1
    vectors[singletons] <- plyr::llply(vectors[singletons], rep, lengths)

    # Interleave vectors
    n <- lengths
    p <- length(vectors)
    interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
    unlist(vectors, recursive = FALSE)[interleave]
}

matched_aes <- function(layer, guide, defaults) {
    all <- names(c(layer$mapping, if (layer$inherit.aes) defaults, layer$stat$default_aes))
    geom <- c(layer$geom$required_aes, names(layer$geom$default_aes))
    matched <- intersect(intersect(all, geom), names(guide$key))
    matched <- setdiff(matched, names(layer$geom_params))
    setdiff(matched, names(layer$aes_params))
}

rename_aes <- function(x) {
    names(x) <- ggplot2::standardise_aes_names(names(x))
    duplicated_names <- names(x)[duplicated(names(x))]
    if (length(duplicated_names) > 0L) {
        duplicated_message <- paste0(unique(duplicated_names), collapse = ", ")
        warning(
            "Duplicated aesthetics after name standardisation: ", duplicated_message, call. = FALSE
        )
    }
    x
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
          "col__", "row__", "t1", "z1", "z2"))
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

.pasteand <- function(x) {
    l <- length(x)
    paste0(paste0(x[-l], collapse = ", ")," and ", x[l])
}

checkListSameLengh <- function(x, names = "x") {
    l <- lengths(x)
    if (slow_equal(l)) {
        return(TRUE)
    }

    if (isTRUE(names)) {
        names <- .pasteand(names(x))
    } else {
        names <- paste0("Elements of ", names)
    }

    return(paste0(names, " must have the same length"))
}

assertListSameLength <- checkmate::makeAssertionFunction(checkListSameLengh)

# fast_equal = inline::cxxfunction(signature(x = 'numeric', y = 'numeric'), '
#   NumericVector var(x);
#   double precision = as<double>(y);
#
#   for (int i = 0, size = var.size(); i < size; ++i) {
#     if (var[i] - var[0] > precision || var[0] - var[i] > precision)
#       return Rcpp::wrap(false);
#   }
#
#   return Rcpp::wrap(true);
# ', plugin = 'Rcpp')

slow_equal <- function(x) diff(range(x)) < sqrt(.Machine$double.eps)

checkSameLength <- function(x) {
    if (!slow_equal(x)) {
        return(paste0(.pasteand(names(x)), " must have the same length"))
    }
    return(TRUE)
}

assertSameLength <- checkmate::makeAssertionFunction(checkSameLength)

checkDateish <- function(x, ...) {
    x <- try(as.Date(x), TRUE)
    if (is.error(x)) {
        return("Must be coercible to date")
    }
    checkDate(x, ...)
}

assertDateish <- checkmate::makeAssertionFunction(checkDateish)
# nocov end

