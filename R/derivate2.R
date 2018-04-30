Derivate.old <- function(formula, data = NULL, order = 1, cyclical = FALSE, fill = FALSE,
                     sphere = FALSE, a = 6371000) {
    dep.names <- formula.tools::lhs.vars(formula)
    ind.names <- formula.tools::rhs.vars(formula)
    formula <- Formula::as.Formula(formula)
    data <- as.data.table(eval(quote(model.frame(formula, data  = data))))

    # Order data.
    id.name <- digest::digest(data[, 1])
    data[, (id.name) := 1:.N]
    # data[, id := 1:.N]    # for order.
    setkeyv(data, ind.names)

    if (length(ind.names) > 1) {
        if (length(cyclical) == 1) {
            cyclical <- rep(cyclical, length(ind.names))
        } else if (length(cyclical) < length(ind.names)) {
            stop("One boundary condition per variable needed.")
        }
    }

    dernames <- lapply(ind.names, FUN = function(x) {
        paste0(dep.names, ".",
               paste0(rep("d", order[1]), collapse = ""),
               x)
    })

    for (v in seq_along(ind.names)) {
        this.var <- ind.names[v]
        this.bc <- cyclical[v]

        data[, dernames[[v]] := lapply(seq(dernames[[1]]), function(x) {
            .derv(.SD[[x]], .SD[[this.var]], order = order[1],
                  cyclical = this.bc, fill = fill)}),
            by = c(ind.names[-v])]
    }
    data <- data[order(data[[id.name]])]

    # Correction for spherical coordinates.
    if (sphere == TRUE) {
        cosvar <- cos(data[, get(ind.names[2])]*pi/180)
        data[, dernames[[1]] := get(dernames[[1]])*(180/pi/(a*cosvar))^order[1]]
        data[, dernames[[2]] := get(dernames[[2]])*(180/pi/a)^order[1]]
    }

    data <- data[, unlist(dernames), with = FALSE]

    return(as.list(data))

}
