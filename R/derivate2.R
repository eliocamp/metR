.Derivate <- function(formula, data = NULL, order = 1, cyclical = FALSE, fill = FALSE,
                      sphere = FALSE, a = 6371000) {
    dep.names <- formula.tools::lhs.vars(formula)
    ind.names <- formula.tools::rhs.vars(formula)

    if (!is.null(data)) {
        data <- setDT(data)[, c(dep.names, ind.names), with = FALSE]
    } else {
        data <- as.data.table(mget(c(dep.names, ind.names), envir = environment(formula)))
    }

    if (length(ind.names) > 1) {
        if (length(cyclical) == 1) {
            cyclical <- rep(cyclical, length(ind.names))
        } else if (length(cyclical) < length(ind.names)) {
            stop("One boundary condition per variable needed.")
        }
    }

        dernames <- lapply(dep.names, FUN = function(x) {
            paste0(x, ".",
                   paste0(rep("d", order[1]), collapse = ""),
                   ind.names)
        })
        coords <- lapply(ind.names, function(x) unique(data[[x]]))
        for (v in seq_along(dep.names)) {
            data.array <- array(data[[dep.names[v]]], dim = unlist(lapply(coords, length)))
            s <- lapply(seq_along(coords), function(x) {
                c(.derv.array(data.array, coords, x, order = order[1],
                              cyclical = cyclical[1], fill = fill))
            })
            set(data, NULL, dernames[[v]], s)
        }


    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        cosvar <- cos(data[, get(ind.names[2])]*pi/180)
        data[, dernames[[1]] := get(dernames[[1]])*(180/pi/(a*cosvar))^order[1]]
        data[, dernames[[2]] := get(dernames[[2]])*(180/pi/a)^order[1]]
    }

    data <- data[, unlist(dernames), with = FALSE]

    return(as.list(data))
}

# Derivates multidimensional arrays.
.derv.array <- function(X, coords, margin, order = 1, cyclical = FALSE, fill = FALSE) {
    if (is.vector(x)) {
        return(.derv(X, coords[[1]], order = order, cyclical = cyclical, fill = fill))
    }
    dims <- seq(dim(X))
    coord <- coords[[margin]]
    margins <- dims[!(dims %in% margin)]
    f <- apply(X, margins, function(x) .derv(x, coord, order = order,
                                             cyclical = cyclical, fill = fill))
    f <- aperm(f, c(margin, margins))
    return(f)
}
