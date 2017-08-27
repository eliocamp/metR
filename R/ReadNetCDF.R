#' Read NetCDF files.
#'
#' Using the \code{\link[ncdf4]{ncdf4-package}} package, it reads an .nc file. The advantage
#' over using \code{\link[ncdf4]{ncvar_get}} is that the output is a tidy data.table
#' with proper dimensions.
#'
#' @param file File to read from.
#' @param vars A character vector with the name of the variables to read. If
#' \code{NULL}, then it read all the variables.
#' @param list.vars If \code{TRUE}, then it only list available variables instead of reading
#' their values.
#'
#' @return If \code{list.vars} is \code{FALSE} (de default) then it returns a
#' data.table in which each column is a variable and each rown an observation. \cr
#' Else, it returns a character vector with the names of the variables
#' (not the dimensions) included in the file.
#'
#'
#' @examples
#' \dontrun{
#' file <- "file.nc"
#' # Get a list of variables.
#' variables <- ReadNetCDF(file, list.vars = T)
#' # Read only the first one.
#' field <- ReadNetCDF(file, vars = variables[1])
#' }
#'
#' @export
#' @importFrom lubridate years weeks days hours minutes seconds milliseconds ymd_hms
#' @import data.table
ReadNetCDF <- function(file, vars = NULL, list.vars = F) {
    # Usa la librería netcdf para leer archivos y organiza todo en un data.table
    # Entra:
    #   file: la ruta del archivo
    #   vars: las variables a leer. Si es NULL, lee todas.
    #   list.vars: leer los datos o sólo listar las variables y dimensiones
    # Sale:
    #   si list.vars == F, un elemento de clase data.table con las variables
    #   en cada columna.
    #   si list.vars == T, una lista con el nombre de las variables y las
    #   dimensiones.

    ncfile <- ncdf4::nc_open(file)

    if (is.null(vars)) {
        vars <- names(ncfile$var)
    }
    # Leo las dimensiones
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

    if (list.vars == T) {
        r <- list(vars = vars, dimensions = dimensions)
        return(r)
    }

    # Leo la primera variable para luego hacer melt y obtener el data.table
    # al que luego le agrego las otras variables
    var1 <- ncdf4::ncvar_get(ncfile, vars[1], collapse_degen = FALSE)
    order <- ncfile$var[[vars[1]]]$dimids
    dimensions <- dimensions[dims[as.character(order)]]
    dimnames(var1) <- dimensions
    nc <- data.table::melt(var1, varnames = names(dimensions), value.name = vars[1])
    setDT(nc)
    if ("time" %in% names(dimensions)) {
        nc[, date := as.Date(time[1]), by = time]
        nc[, time := NULL]
    }
    if (length(vars) > 1) {
        # Otras variables.
        nc[, c(vars[-1]) := lapply(vars[-1], ncdf4::ncvar_get, nc = ncfile)]
    }
    # Dejemos todo prolijo antes de salir.
    ncdf4::nc_close(ncfile)
    return(nc)
}
