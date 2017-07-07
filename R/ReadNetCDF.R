#' Read NetCDF files.
#'
#' Using the \code{\link{ncdf4}} package, it reads an .nc file. The advantage
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

    library(ncdf4)
    library(data.table)
    ncfile <- nc_open(file)

    if (is.null(vars)) {
        vars <- names(ncfile$var)
    }
    # Leo las dimensiones
    dims <- names(ncfile$dim)
    dims <- dims[dims != "nbnds"]
    ids <- vector()
    dimensions <- list()
    for (i in seq_along(dims)) {
        dimensions[[dims[i]]] <- ncvar_get(ncfile, dims[i])
        ids[i] <- ncfile$dim[[i]]$id
    }
    names(dims) <- ids


    if ("time" %in% names(dimensions)) {
        date.unit <- ncfile$dim$time$units
        date.unit <- strsplit(date.unit, " since ", fixed = TRUE)[[1]]
        library(lubridate)
        date.fun <- match.fun(date.unit[1])
        dimensions[["time"]] <- as.character(ymd_hms(date.unit[2]) + date.fun(dimensions[["time"]]))
    }

    if (list.vars == T) {
        r <- list(vars = vars, dimensions = dimensions)
        return(r)
    }

    # Leo la primera variable para luego hacer melt y obtener el data.table
    # al que luego le agrego las otras variables
    var1 <- ncvar_get(ncfile, vars[1], collapse_degen = FALSE)
    order <- ncfile$var[[vars[1]]]$dimids
    dimensions <- dimensions[dims[as.character(order)]]
    dimnames(var1) <- dimensions
    nc <- melt(var1, varnames = names(dimensions), value.name = vars[1])
    setDT(nc)
    if ("time" %in% names(dimensions)) {
        nc[, date := as.Date(time[1]), by = time]
        nc[, time := NULL]
    }
    if (length(vars) > 1) {
        nc[, c(vars[-1]) := lapply(vars[-1], ncvar_get, nc = ncfile)]    # otras variables
    }
    # Dejemos todo prolijo antes de salir.
    nc_close(ncfile)
    return(nc)
}
