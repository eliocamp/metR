# nocov start


# Helper functions for translation
catf = function(fmt, ..., sep = " ", domain = "R-metR") {
    cat(gettextf(fmt, ..., domain = domain), sep = sep)
}

stopf <- function(fmt, ..., call. = TRUE, domain = "R-metR") {
    x <- gettextf(fmt, ..., domain = domain)
    if (isTRUE(call.)) {
        call = sys.call(-1)
    } else {
        call = NULL
    }
    e <- simpleError(x, call = call)
    stop(e)
}


warningf <- function (fmt, ..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
                      domain = "R-metR") {
    x <- gettextf(fmt, ..., domain = domain)
    if (isTRUE(call.)) {
        call = sys.call(-1)
    } else {
        call = NULL
    }
    e <- simpleWarning(x, call = call)
    warning(e)
}


messagef = function(fmt, ..., appendLF = TRUE, domain = "R-metR") {
    message(gettextf(fmt, ..., domain = domain), domain = NA, appendLF = appendLF)
}


.tidy2matrix <- function(data, formula, value.var, fill = NULL, ...) {
    # browser()
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])
    data <- data.table::as.data.table(data)
    data[, row__ := .GRP, by = c(row.vars)]
    data[, col__ := .GRP, by = c(col.vars)]
    if (is.null(fill)) {
        fill <- 0
        # rowdims <- data[col__ == 1, (row.vars), with = FALSE]
        # coldims <- data[row__ == 1, (col.vars), with = FALSE]
    } else {
        # rowdims <- unique(data[, (row.vars), with = FALSE])
        # coldims <- unique(data[, (col.vars), with = FALSE])
    }
    rowdims <- unique(data[, (row.vars), with = FALSE])
    coldims <- unique(data[, (col.vars), with = FALSE])
    data.m <- matrix(fill[1], nrow = max(data[["row__"]]),
                     ncol = max(data[["col__"]]))
    data.m[cbind(data[["row__"]], data[["col__"]])] <- data[[value.var]]

    return(list(matrix = data.m,
                coldims = coldims,
                rowdims = rowdims))
}


seq_range <- function(x, by = ggplot2::resolution(x, zero = FALSE), ...) {
    r <- range(x)
    seq.int(r[1], r[2], by = by, ...)
}

is.error <- function(x) inherits(x, "try-error")



# from data.table
#' @importFrom data.table %chin%
guess <- function (x) {
    if ("value" %chin% names(x))
        return("value")
    if ("(all)" %chin% names(x))
        return("(all)")
    var <- names(x)[ncol(x)]
    messagef("Using \"%s\" as value column. Use 'value.var' to override", var)
    return(var)
}

# from ggplot2
`%||%` <- function(a, b) {
    if (!is.null(a)) a else b
}
# from ggplot2
is.waive <- function(x) {
    inherits(x, "waiver")
}

# from ggplot2s
waiver <- function ()
    structure(list(), class = "waiver")

element_render <- function(theme, element, ..., name = NULL) {

    # Get the element from the theme, calculating inheritance
    el <- ggplot2::calc_element(element, theme)
    if (is.null(el)) {
        messagef("Theme element %s missing", element)
        return(ggplot2::zeroGrob())
    }

    grob <- ggplot2::element_grob(el, ...)
    ggname(paste(element, name, sep = "."), grob)
}

ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}


width_cm <- function(x) {
    if (grid::is.grob(x)) {
        grid::convertWidth(grid::grobWidth(x), "cm", TRUE)
    } else if (grid::is.unit(x)) {
        grid::convertWidth(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, width_cm, numeric(1))
    } else {
        stopf("Unknown input.")
    }
}
height_cm <- function(x) {
    if (grid::is.grob(x)) {
        grid::convertHeight(grid::grobHeight(x), "cm", TRUE)
    } else if (grid::is.unit(x)) {
        grid::convertHeight(x, "cm", TRUE)
    } else if (is.list(x)) {
        vapply(x, height_cm, numeric(1))
    } else {
        stopf("Unknown input.")
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
    do.call("grid::unit.c", do.call("interleave.default", plyr::llply(list(...), as.list)))
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
        warningf("Duplicated aesthetics after name standardisation: %s", duplicated_message, call. = FALSE)
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
          "col__", "row__", "t1", "z1", "z2", "..n", ".N", ":=", ".SD", ".I", ".GRP"))
}


.has_single_value <- function(data, coords) {
    lengths <- data[, .N, by = coords]$N
    !any(lengths > 1)
}


.is.regular_grid <- function(x, y) {
    data <- data.table::data.table(x = x, y = y)
    nx <- data[, data.table::uniqueN(x), by = y]$V1
    ny <- data[, data.table::uniqueN(y), by = x]$V1

    xs <- data.table::uniqueN(data$x)
    ys <- data.table::uniqueN(data$y)


    # Conditinos for regular grid
    # 1. each y has the same number of unique values of x
    # 2. each x has the same number of unique values of y
    regularity <- sum(abs(ys - ny)) == 0 & sum(abs(xs - nx)) == 0

    # 3. there are no duplicated values
    lengths <- data[, .N, by = .(x, y)]$N
    unicity <- !any(lengths > 1)

    regularity & unicity
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


checkURLFile <- function(x) {
    access <- unname(file.access(x, 4) == 0 | RCurl::url.exists(x))

    if (isFALSE(access)) {
        return("File or URL not readable")
    }
    return(access)
}

assertURLFile <- checkmate::makeAssertionFunction(checkURLFile)


check_packages <- function(packages, fun) {
    installed <- vapply(packages, function(p) {
        requireNamespace(p, quietly = TRUE)
    }, TRUE)

    missing <- packages[!installed]

    if (length(missing != 0)) {
        stopf("%s needs packages %s. Install them with: 'install.packages(c(\"%s\"))'.",
              fun, paste0(missing, collapse = ", "), paste0(missing, collapse = "\", \""))
    }
}

.datatable.aware <- TRUE

a <- 6371000


smooth2d <- function(x, y, value, kx = 1, ky = 1) {
    data <- data.table::data.table(x, y, value)
    # browser()
    g <- .tidy2matrix(data, x ~ y, value.var = "value")

    f <- fft(g$matrix)
    f1 <- f

    kx <- c(0, seq_len(floor(nrow(f)/2*kx)))
    kx <- c(kx + 1, nrow(f) - kx[kx != 0] + 1)

    ky <- c(0, seq_len(floor(ncol(f)/2*ky)))
    ky <- c(ky + 1, ncol(f) - ky[ky != 0] + 1)

    f1[, -ky] <- 0
    f1[-kx, ] <- 0

    c(Re(fft(f1, inverse = TRUE)/length(f1)))
}


downsample <- function(x, y, value, byx = 1, byy = 1, fill = mean) {
    data <- data.table::data.table(x, y, value)
    browser()
    fill <- mean(value, na.rm = TRUE)
    g <- .tidy2matrix(data, x ~ y, value.var = "value", fill = fill)
    g$matrix[is.na(g$matrix)] <- fill
    f <- fft(g$matrix)
    f1 <- f

    kx <- 1/byx
    ky <- 1/byy

    kx <- c(0, seq_len(floor(nrow(f)/2*kx)))
    kx <- c(kx + 1, nrow(f) - kx[kx != 0] + 1)

    ky <- c(0, seq_len(floor(ncol(f)/2*ky)))
    ky <- c(ky + 1, ncol(f) - ky[ky != 0] + 1)

    f1[, -ky] <- 0
    f1[-kx, ] <- 0

    data$value_smooth <- c(Re(fft(f1, inverse = TRUE)/length(f1)))

    data <- subset(data, x %in% JumpBy(sort(unique(x)), byx) &
                       y %in% JumpBy(sort(unique(y)), byy))
    data
}

# nocov end




