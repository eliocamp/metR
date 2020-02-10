#' Read NetCDF files.
#'
#' Using the \code{\link[ncdf4]{ncdf4-package}} package, it reads a NetCDF file. The advantage
#' over using \code{\link[ncdf4]{ncvar_get}} is that the output is a tidy data.table
#' with proper dimensions.
#'
#' @param file file to read from.
#' @param vars a character vector with the name of the variables to read. If
#' \code{NULL}, then it read all the variables.
#' @param out character indicating the type of output desired
#' @param subset a list of subsetting objects. See below.
#' @param key if `TRUE`, returns a data.table keyed by the dimensions of the data.
#' @param ... ignored. Is there for convenience so that a call to [ReadNetCDF()] can
#' be also valid for [GlanceNetCDF()].
#'
#' @section Subsetting:
#' In the most basic form, `subset` will be a named list whose names must match
#' the dimensions specified in the NetCDF file and each element must be a vector
#' whose range defines
#' a contiguous subset of data. You don't need to provide and exact range that
#' matches the actual gridpoints of the file; the closest gridpoint will be selected.
#' Furthermore, you can use `NA` to refer to the existing minimum or maximum.
#'
#' So, if you want to get Southern Hemisphere data from the from a file that defines
#' latitude as `lat`, then you can use:
#' \preformatted{
#' subset = list(lat = -90:0)
#'}
#' More complex subsetting operations are supported. If you want to read non-contiguous
#' chunks of data, you can specify each chunk into a list inside `subset`. For example
#' this subset
#' \preformatted{
#' subset = list(list(lat = -90:-70, lon = 0:60),
#'               list(lat = 70:90, lon = 300:360))
#'}
#' will return two contiguous chunks: one on the South-West corner and one on the
#' North-East corner. Alternatively, if you want to get the four corners that
#' are combination of those two conditions,
#'
#' \preformatted{
#' subset = list(lat = list(-90:-70, 70:90),
#'               lon = list(0:60, 300:360))
#'}
#' Both operations can be mixed together. So for example this
#'
#' \preformatted{
#' subset = list(list(lat = -90:-70,
#'                    lon = 0:60),
#'               time = list(c("2000-01-01", "2000-12-31"),
#'                           c("2010-01-01", "2010-12-31")))
#'}
#'
#' returns one spatial chunk for each of two temporal chunks.
#'
#' The general idea is that named elements define 'global' subsets ranges that will be
#' applied to every other subset, while each unnamed element define one contiguous chunk.
#' In the above example, `time` defines two temporal ranges that every subset of data will
#' have.
#'
#' The above example, then, is equivalent to
#'
#' \preformatted{
#' subset = list(list(lat = -90:-70,
#'                    lon = 0:60,
#'                    time = c("2000-01-01", "2000-12-31")),
#'               list(lat = -90:-70,
#'                    lon = 0:60,
#'                    time = c("2010-01-01", "2010-12-31")))
#'}
#'
#' but demands much less typing.
#'
#' @return
#' The return format is specified by `out`. It can be a data table in which each
#' column is a variable and each row, an observation; an array with named
#' dimensions; or a vector. Since it's possible to return multiple arrays or
#' vectors (one for each variable), for consistency the return type is always a
#' list. Either of these two options are much faster than the
#' first since the most time consuming part is the melting of the array
#' returned by [ncdf4::ncvar_get]. `out = "vector"` is particularly useful for
#' adding new variables to an existing data frame with the same dimensions.
#'
#' When not all variables specified in `vars` have the same number of dimensions,
#' the shorter variables will be recycled. E.g. if reading a 3D pressure field
#' and a 2D surface temperature field, the latter will be turned into a 3D field
#' with the same values in each missing dimension.
#'
#' `GlanceNetCDF()` returns a list of variables and dimensions included in the
#' file with a nice printing method.
#'
#' @examples
#' \dontrun{
#' file <- "file.nc"
#' # Get a list of variables.
#' variables <- GlanceNetCDF(file)
#' print(variables)
#'
#' # Read only the first one, with name "var".
#' field <- ReadNetCDF(file, vars = c(var = names(variables$vars[1])))
#' # Add a new variable.
#' # ¡Make sure it's on the same exact grid!
#' field[, var2 := ReadNerCDF(file2, out = "vector", subset = list(lat = 90:10))]
#' }
#'
#' @export
#' @importFrom lubridate years weeks days hours minutes seconds milliseconds ymd_hms
#' @import data.table
ReadNetCDF <- function(file, vars = NULL,
                       out = c("data.frame", "vector", "array"),
                       subset = NULL, key = FALSE) {
    ncdf4.available <- requireNamespace("ncdf4", quietly = TRUE)
    if (!ncdf4.available) {
        stop("ReadNetCDF needs package'ncdf4'. ",
             "Install it with 'install.packages(\"ncdf4\")'")
    }


    out <- out[1]
    checks <- makeAssertCollection()
    assertCharacter(file, len = 1, min.chars = 1, any.missing = FALSE, add = checks)
    assertAccess(file, "r", add = checks)
    assertCharacter(vars, null.ok = TRUE, any.missing = FALSE, unique = TRUE,
                    add = checks)
    assertChoice(out, c("data.frame", "vector", "array", "vars"), add = checks)
    assertList(subset, types = c("vector", "POSIXct", "POSIXt", "Date", "list"), null.ok = TRUE, add = checks)
    # assertNamed(subset, c("unique"), add = checks)
    assertFlag(key, add = checks)

    reportAssertions(checks)

    ncfile <- ncdf4::nc_open(file)

    if (out[1] == "vars") {
        r <- list(vars = ncfile$var,
                  dims = ncfile$dim)
        class(r) <- c("nc_glance", class(r))
        # options(OutDec = dec)
        return(r)
    }

    dec <- getOption("OutDec")
    # Dejemos todo prolijo antes de salir.
    options(OutDec = ".")
    on.exit({
        options(OutDec = dec)
        ncdf4::nc_close(ncfile)
    })

    if (is.null(vars)) {
        vars <- names(ncfile$var)
    }

    # Vars must be a (fully) named vector.
    varnames <- names(vars)
    if (is.null(varnames)) {
        names(vars) <- vars
    } else {
        no.names <- nchar(varnames) == 0
        names(vars)[no.names] <- vars[no.names]
    }

    # Leo las dimensiones.
    dims <- names(ncfile$dim)
    # dims <- dims[dims != "nbnds"]
    ids <- vector()
    dimensions <- list()
    for (i in seq_along(dims)) {
        dimensions[[dims[i]]] <- ncfile$dim[[dims[i]]]$vals
        ids[i] <- ncfile$dim[[i]]$id
    }
    names(dims) <- ids
    has_timedate <- FALSE
    if ("time" %in% names(dimensions) && ncfile$dim$time$units != "") {
        time <- .parse_time(dimensions[["time"]],
                            ncfile$dim$time$units)
        dimensions[["time"]] <- as.character(time)
        if (.is.somedate(time)) {
            has_timedate <- TRUE
        }
    }

    # if (out[1] == "vars") {
    #     r <- list(vars = unname(vars), dimensions = dimensions, dims = dims)
    #     # options(OutDec = dec)
    #     return(r)
    # }

    ## Hago los subsets
    # Me fijo si faltan dimensiones
    subset <- .expand_chunks(subset)

    subset_names <- .names_recursive(subset)
    subset.extra <- subset_names[!(subset_names %in% names(dimensions))]
    if (length(subset.extra) != 0) {
        stop(paste0("Subsetting dimensions not found: ",
                    paste0(subset.extra, collapse = ", "), "."))
    }

    if (length(subset) > 1) {
        if (out != "data.frame") {
            stop('Multiple subsets only supported for `out = "data.frame"')
        }
        reads <- lapply(subset, function(this_subset) {
            ReadNetCDF(file = file, vars = vars, out = out, key = key, subset = this_subset)
        })
        return(data.table::rbindlist(reads))
    } else {
        subset <- subset[[1]]
    }

    # Leo las variables y las meto en una lista.
    nc <- list()
    nc_dim <- list()

    dim.length <- vector("numeric", length = length(vars))

    for (v in seq_along(vars)) {
        # Para cada variable, veo start y count
        order <- ncfile$var[[vars[v]]]$dimids
        start <- rep(1, length(order))
        names(start) <- names(dimensions[dims[as.character(order)]])
        count <- rep(-1, length(order))
        names(count) <- names(dimensions[dims[as.character(order)]])

        sub.dimensions <- dimensions

        for (s in names(subset)[names(subset) %in% names(start)]) {
            d <- dimensions[[s]]
            sub <- subset[[s]]

            if (is.na(sub[1])) {
                sub[1] <- min(d)
            }

            if (is.na(sub[2])) {
                sub[2] <- max(d)
            }

            if (.is.somedate(sub) | s == "time") {
                start[[s]] <- which(lubridate::as_datetime(d) %~% min(lubridate::as_datetime(sub)))
                count[[s]] <- abs(which(lubridate::as_datetime(d) %~% max(lubridate::as_datetime(sub))) - start[[s]] + 1)
            } else {
                start1 <- which(d %~% sub[1])
                end <- which(d %~% sub[length(sub)])
                start[[s]] <- min(start1, end)
                count[[s]] <- abs(end - start1) + 1
            }

            if(count[[s]] == 0) count[[s]] <- 1

            sub.dimensions[[s]] <- dimensions[[s]][seq.int(start[[s]], start[[s]] + count[[s]] - 1)]
        }


        var1 <- ncdf4::ncvar_get(ncfile, vars[v], collapse_degen = FALSE, start = start,
                                 count = count)
        dimnames(var1) <- sub.dimensions[dims[as.character(order)]]


        dim.length[v] <- length(order)
        nc[[v]] <- var1
        nc_dim[[v]] <- as.vector(sub.dimensions[dims[as.character(order)]])

    }

    if (out[1] == "array") {
        return(nc)
    } else if (out[1] == "vector") {
        nc <- lapply(seq_along(nc), function(x) c(nc[[x]]))
        names(nc) <- names(vars)
        return(nc)
    } else {
        first.var <- which.max(dim.length)
        nc.df <- .melt_array(nc[[first.var]], dims = nc_dim[[first.var]],
                             value.name = names(vars)[first.var])

        if ("time" %in% names(dimensions) && has_timedate) {
            nc.df[, time2 := lubridate::as_datetime(time[1]), by = time]
            nc.df[, time := NULL]
            data.table::setnames(nc.df, "time2", "time")
        }

        for (v in seq_along(vars)[-first.var]) {
            this.dim <- names(dimnames(nc[[v]]))
            first.dim <- names(dimnames(nc[[first.var]]))
            missing.dim <- first.dim[!(first.dim %in% this.dim)]
            ..n <- c(nc[[v]])
            nc.df[, names(vars)[v] := ..n, by = c(missing.dim)]
        }

        if (key == TRUE) data.table::setkeyv(nc.df, names(nc.df)[!(names(nc.df) %in% names(vars))])
    }


    return(nc.df[])
}

.parse_time <- function(time, units) {
    if (!requireNamespace("udunits2", quietly = TRUE)) {
        message("Time dimension found and package udunits2 is not installed. Trying to parse.")
        fail <- paste0("Time parsing failed. Returing raw values in ", units, ".\n",
                       "Install udunits2 with `install_packages(\"udunits2\")` to parse it automatically.")

        units <- trimws(strsplit(units, "since")[[1]])

        period_fun <- try(match.fun(units[1]), silent = TRUE)
        if (is.error(period_fun)) {
            warning(fail)
            return(time)
        }

        time_try <- try(as.POSIXct(units[2],  tz = "UTC", origin = "1970-01-01 00:00:00") +
                            period_fun(time),
                        silent = TRUE)
        if (is.error(time_try)) {
            warning(fail)
            return(time)
        }
        return(time_try)
    }

    time <- udunits2::ud.convert(time, units,
                                 "seconds since 1970-01-01 00:00:00")
    as.POSIXct(time, tz = "UTC", origin = "1970-01-01 00:00:00")
}

.expand_chunks <- function(subset) {
    if (is.null(subset)) {
        return(list(NULL))
    }
    # Make everything a list
    subset <- lapply(subset, function(l) {
        if (!is.list(l)) {
            list(l)
        } else {
            l
        }
    })

    if (is.null(names(subset))) {
        names(subset) <- rep("", length(subset))
    }
    # If it has name, is a global subset,
    # otherwhise, is a chunck definition
    has_name <- names(subset) != ""

    new_subset <- subset[has_name]
    if (sum(!has_name) != 0) new_subset["chunks"] <- list(subset[!has_name])

    chunks <- purrr::cross(new_subset)
    new_subset <- lapply(chunks, function(chunk) {
        is.chunk <- which(names(chunk) == "chunks")
        if (length(is.chunk) != 0) {
            c(chunk[-is.chunk], chunk[[is.chunk]])
        } else {
            chunk
        }
    })

    to_range <- function(x) {
        if (is.list(x)) {
            lapply(x, to_range)
        } else {
            if (length(x) != 2) {
                range(x)
            } else {
                x
            }
        }
    }

    new_subset <- lapply(new_subset, to_range)
    new_subset
}


.names_recursive <- function(x) {
    out <- names(x)
    if (is.list(x)) {
        out <- c(out, unlist(lapply(x, .names_recursive)))
    }
    unique(out[out != ""])
}

#' @rdname ReadNetCDF
#'
#' @export
GlanceNetCDF <- function(file, ...) {
    ReadNetCDF(file, out = "vars")
}

#' @export
print.nc_glance <- function(x, ...) {
    cat("----- Variables ----- \n")
    out <- lapply(x$vars, print)

    cat("\n\n")
    cat("----- Dimensions ----- \n")
    out <- lapply(x$dim, print)
}

#' @export
print.ncvar4 <- function(x, ...) {
    cat(x$name, ":\n", sep = "")
    cat("    ", x$longname, sep = "")

    if (x$units != "") cat(" in ", x$units, sep = "")

    cat("\n")
    dims <- vapply(x$dim, function(x) x$name, "a")

    cat("    Dimensions: ")
    cat(paste0(dims, collapse = " by "), sep = "")
    cat("\n")

    return(invisible(x))
}

#' @export
print.ncdim4 <- function(x, ...) {
    # cat("$", dim$name, "\n", sep = "")
    if (x$name == "time" & x$units != "") {
        vals <- suppressMessages(suppressWarnings(.parse_time(x$vals, x$units)))

        if (.is.somedate(vals)) {
            units <- ""
        }


    } else {
        vals <- x$vals
        units <- x$units
    }

    cat("  ", x$name, ": ",
        x$len, " values from ",
        as.character(min(vals)), " to ",
        as.character(max(vals)), " ",
        units,"\n", sep = "")
    return(invisible(x))
}


.melt_array <- function(array, dims, value.name = "V1") {
    dims <- lapply(dims, c)
    dims <- c(dims[length(dims):1], sorted = FALSE)
    grid <- do.call(data.table::CJ, dims)
    grid[, c(value.name) := c(array)][]

    return(grid)
}
