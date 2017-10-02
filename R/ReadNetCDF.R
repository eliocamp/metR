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
#' column is a variable and each rown an observation; an array with named
#' dimensions; or a vector. Either of these two options are much faster than the
#' first since the most time consuming part is the melting of the array
#' returned by [ncdf4::ncvar_get]. `out = "vector"` is particularly usefull for
#' adding new variables to an existing data frame with the same dimensions. Note
#' that only one variable can be retrieved at a time with these formats.
#'
#' Finally, it can also be `vars`, in which case it returns a list with the name
#' of the available variables and the dimensions of the spaciotemporal grid.
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
#' @import data.table
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
        no.names <- sapply(varnames, nchar) == 0
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
        date.unit <- ncfile$dim$time$units
        date.unit <- strsplit(date.unit, " since ", fixed = TRUE)[[1]]
        date.fun <- get(paste0(date.unit[1]))
        dimensions[["time"]] <- as.character(lubridate::ymd_hms(date.unit[2]) +
                                                 date.fun(dimensions[["time"]]))
    }

    if (out[1] == "vars") {
        r <- list(vars = vars, dimensions = dimensions)
        return(r)
    }

    # Leo la primera variable para luego hacer melt y obtener el data.table
    # al que luego le agrego las otras variables
    var1 <- ncdf4::ncvar_get(ncfile, vars[1], collapse_degen = FALSE)
    order <- ncfile$var[[vars[1]]]$dimids
    dimensions <- dimensions[dims[as.character(order)]]
    dimnames(var1) <- dimensions
    if (out[1] == "array") {
        ncdf4::nc_close(ncfile)
        return(var1)
    } else if (out[1] == "vector") {
        ncdf4::nc_close(ncfile)
        return(c(var1))
    } else {
        nc <- data.table::melt(var1, varnames = names(dimensions), value.name = names(vars)[1])
        data.table::setDT(nc)

        if ("time" %in% names(dimensions)) {
            nc[, date := as.POSIXct(time[1]), by = time]
            nc[, time := NULL]
        }
        if (length(vars) > 1) {
            # Otras variables.
            nc[, c(names(vars[-1])) := lapply(vars[-1], ncdf4::ncvar_get, nc = ncfile)]
        }
        # Dejemos todo prolijo antes de salir.
        ncdf4::nc_close(ncfile)
        return(nc)
    }
}
