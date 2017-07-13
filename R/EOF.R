#' Empirical Orthogonal Function
#'
#' Uses \code{\link[svd]{propack.svd}} to compute Empirical Orthogonal Functions
#' (aka Singular Value Decomposition).
#'
#' @param data a data.frame
#' @param formula formula parsed to \code{\link[data.table]{dcast}} to build
#' the matrix that will be used in the SVD decomposition (see details)
#' @param value.var name of the column whose values will be used
#' @param n which singular values to return
#'
#' @return
#' A list of 3 named elements containing tidy data.tables of the right and left
#' singular vectors, and a named vector of singular values.
#'
#' @details
#' Singular values can be computed over matrices so \code{formula} denotes how
#' to build a matrix from the data. It is a formula of the form LHS ~ RHS in
#' which LHS represent the variables used to make the rows and RHS, the columns
#' of the matrix. The result of LHS ~ RHS and RHS ~ LHS (ie, terms reversed)
#' is the same, with the exception that the order of the singular vector is
#' reversed (right is left and left is right).
#'
#' The variable combination used in this formula *must* identify
#' an unique value in a cell. For the time being, no error will be raised, but
#' there will be a message from \code{\link[data.table]{dcast}}.
#'
#' \code{\link[svd]{propack.svd}} does not accept \code{NA}s, so each combination
#' of the variables in \code{formula} must be a valid numeric value.
#'
#' @examples
#' # The Antarctic Oscillation is computed from the
#' monthly geopotential height anomalies weigthed by latitude.
#' aao[, gh.t.w := Anomaly(gh)*sqrt(cos(lat*pi/180)),
#'       by = .(lon, lat, month(date))]
#' aao.svd <- EOF(aao, lat + lon ~ date, value.var = "gh.t.w", n = 1)
#'
#' # AAO field
#' ggplot(aao.svd$left, aes(lon, lat, z = loading.1)) +
#'     geom_contour(aes(color = ..level..)) +
#'     coord_polar()
#'
#' # AAO signal
#' ggplot(aao.svd$right, aes(date, loading.1)) +
#'     geom_line()
#'
#' @family meteorology functions
#' @export
#' @import data.table
#' @import svd
EOF <- function(data, formula, value.var, n = 1) {
    row.vars <- all.vars(formula[[2]])
    col.vars <- all.vars(formula[[3]])

    g <- dcast(setDT(data), formula, value.var = value.var)

    dims <- list()
    if (length(col.vars) > 1) {
        cols <- unlist(strsplit(colnames(g), split = "_"))
    } else {
        cols <- colnames(g)
    }

    for (i in seq_along(col.vars)) {
        dims[[i]] <- JumpBy(cols, length(col.vars), start = i + length(row.vars))
        if (class(data[[col.vars[i]]]) != "Date") {
            dims[[i]] <- as(dims[[i]], class(data[[col.vars[i]]]))
        } else {
            dims[[i]] <- as.Date(dims[[i]])
        }
    }
    names(dims) <- col.vars

    eof <- svd::propack.svd(as.matrix(g[,-seq_along(row.vars), with = F]),
                            neig = max(n))

    right <- as.data.table(eof$v)
    colnames(right) <- paste0("loading.", 1:max(n))
    right <- right[, n, with = FALSE]
    right <- cbind(right, as.data.table(dims))

    left <- as.data.table(eof$u)
    colnames(left) <- paste0("loading.", 1:max(n))
    left <- left[, n, with = FALSE]
    left <- cbind(left, g[, row.vars, with = F])

    sv <- eof$d
    names(sv) <-  paste0("loading.", 1:max(n))
    sv <- sv[n]

    return(list(right = right, left = left, sv = sv))
}
