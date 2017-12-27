#' Read NetCDF files.
#'
#' Using the \code{\link[ncdf4]{ncdf4-package}} package, it reads an .nc file. The advantage
#' over using \code{\link[ncdf4]{ncvar_get}} is that the output is a tidy data.table
#' with proper dimensions.
#'
#' @param file file to read from.
#' @param vars a character vector with the name of the variables to read. If
#' \code{NULL}, then it read all the variables.
#' @param out character indicating the type of output desired
#'
#' @return
#' The return format is specified by `out`. It can be a data table in which each
#' column is a variable and each row, an observation; an array with named
#' dimensions; or a vector. Either of these two options are much faster than the
#' first since the most time consuming part is the melting of the array
#' returned by [ncdf4::ncvar_get]. `out = "vector"` is particularly useful for
#' adding new variables to an existing data frame with the same dimensions. Note
#' that only one variable can be retrieved at a time with these formats.
#'
#' Finally, it can also be `vars`, in which case it returns a list with the name
#' of the available variables and the dimensions of the spatiotemporal grid.
#'
#' When not all variables specified in `vars` have the same number of dimensions,
#' the shorter variables will be recicled. E.g. if reading a 3D pressure field
#' and a 2D surface temperature field, the latter will be turned into a 3D field
#' with the same values in each missing dimension.
#'
#' @examples
#' \dontrun{
#' file <- "file.nc"
#' # Get a list of variables.
#' variables <- ReadNetCDF(file, out = "vars")
#' # Read only the first one, with name "var".
#' field <- ReadNetCDF(file, vars = c(var = variables$vars[1]))
#' # Add a new variable.
#' # ¡Make sure it's on the same exact grid!
#' field[, var2 := ReadNerCDF(file2, out = "vector")]
#' }
#'
#' @export
#' @importFrom lubridate years weeks days hours minutes seconds milliseconds ymd_hms
#' @import data.table udunits2
ReadNetCDF <- function(file, vars = NULL, out = c("data.frame", "vector", "array", "vars")) {
    ncfile <- ncdf4::nc_open(file)

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
    dims <- dims[dims != "nbnds"]
    ids <- vector()
    dimensions <- list()
    for (i in seq_along(dims)) {
        dimensions[[dims[i]]] <- ncdf4::ncvar_get(ncfile, dims[i])
        ids[i] <- ncfile$dim[[i]]$id
    }
    names(dims) <- ids

    if ("time" %in% names(dimensions)) {
        # date.unit <- ncfile$dim$time$units
        # date.unit <- strsplit(date.unit, " since ", fixed = TRUE)[[1]]
        # date.fun <- get(paste0(date.unit[1]))
        # orders <- c("ymd HMS", "ymd HM")
        # dimensions[["time"]] <- as.character(lubridate::parse_date_time(date.unit[2],
        # orders) +
        # date.fun(dimensions[["time"]]))
        time <- udunits2::ud.convert(dimensions[["time"]],
                                     ncfile$dim$time$units,
                                     "seconds since 1970-01-01 00:00:00")
        dimensions[["time"]] <- as.character(as.POSIXct(time, tz = "UTC",
                                                        origin = "1970-01-01 00:00:00"))

    }

    if (out[1] == "vars") {
        r <- list(vars = vars, dimensions = dimensions)
        return(r)
    }

    # Leo las variables y las meto en una lista.
    nc <- list()
    dim.length <- vector("numeric", length = length(vars))
    for (v in seq_along(vars)) {
        var1 <- ncdf4::ncvar_get(ncfile, vars[v], collapse_degen = FALSE)
        order <- ncfile$var[[vars[v]]]$dimids
        dimnames(var1) <- dimensions[dims[as.character(order)]]
        dim.length[v] <- length(order)
        nc[[v]] <- var1
    }

    if (out[1] == "array") {
        ncdf4::nc_close(ncfile)
        return(nc)
    } else if (out[1] == "vector") {
        ncdf4::nc_close(ncfile)
        nc <- lapply(1:length(nc), function(x) c(nc[[x]]))
        names(nc) <- names(vars)
        return(nc)
    } else {
        first.var <- which.max(dim.length)
        nc.df <- data.table::melt(nc[[first.var]], varnames = names(dimnames(nc[[first.var]])),
                                  value.name = names(vars)[first.var])
        data.table::setDT(nc.df)

        if ("time" %in% names(dimensions)) {
            nc.df[, date := lubridate::as_datetime(time[1]), by = time]
            nc.df[, time := NULL]
        }

        for (v in seq_along(vars)[-first.var]) {
            missing.dim <- names(dimensions)[!(names(dimensions) %in% names(dimnames(nc[[v]])))]
            nc.df[, c(names(vars[-first.var])) := lapply(seq_along(vars)[-first.var],
                                                         function(x) c(nc[[x]])),
                  by = c(missing.dim)]
        }

    }
    # Dejemos todo prolijo antes de salir.
    ncdf4::nc_close(ncfile)
    return(nc.df)
}


