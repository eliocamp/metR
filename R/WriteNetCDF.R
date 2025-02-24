WriteNetCDF <- function(data, file, vars, dims = NULL) {

    if (is.null(names(vars))) {
        names(vars) <- vars
    }

    if (any(!names(vars) %in% colnames(data))) {
        stop("vars not found in data")
    }

    if (is.null(dims)) {
        dims <- rev(setdiff(colnames(data), names(vars)))
    }

    if (is.null(names(dims))) {
        names(dims) <- dims
    }

    if (any(!names(dims) %in% colnames(data))) {
        stop("dims not found in data")
    }

    order <- do.call(base::order, args = lapply(rev(names(dims)), function(x) data[[x]]))

    nc_dims <- vector(length(dims), mode = "list")
    for (d in seq_along(dims)) {
        vals <- data[[names(dims)[d]]][order]
        vals <- as.numeric(unique(vals))

        n <- length(vals)
        units <- get_units(data[[names(dims)[d]]])
        nc_dims[[d]] <- ncdf4::ncdim_def(name = dims[d],
                                         units = units,
                                         vals = vals,
                                         unlim = FALSE,
                                         create_dimvar = TRUE,
                                         calendar = NA,
                                         longname = dims[d])
    }

    missing_value <- -999
    nc_vars <- vector(length(vars), mode = "list")
    for (v in seq_along(vars)) {
        nc_vars[[v]] <- ncdf4::ncvar_def(name = vars[v],
                                         units = "",
                                         dim = nc_dims,
                                         longname = vars[v],
                                         missval = missing_value)

    }

    nc_file <- ncdf4::nc_create(file, nc_vars)

    for (v in seq_along(vars)) {
        ncdf4::ncvar_put(nc_file,
                         varid = nc_vars[[v]],
                         vals = data[[names(vars[v])]][order])
    }

    ncdf4::nc_close(nc_file)

    return(invisible(file))
}

#' @keywords internal
get_units <- function(x) {
    UseMethod("get_units")
}

#' @keywords internal
get_units.default <- function(x) {
    return("")
}

#' @keywords internal
get_units.Date <- function(x) {
    return("days since 1970-01-01 00:00:00")
}

#' @keywords internal
get_units.POSIXlt <- function(x) {
    return("seconds since 1970-01-01 00:00:00")
}

#' @keywords internal
get_units.POSIXct <- function(x) {
    return("seconds since 1970-01-01 00:00:00")
}


