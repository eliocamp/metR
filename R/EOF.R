#' Empirical Orthogonal Function
#'
#' Computes Singular Value Decomposition (also known as Principal Components
#' Analysis or Empirical Orthogonal Functions).
#'
#' @param data a data.frame
#' @param formula formula passed to \code{\link[data.table]{dcast}} to build
#' the matrix that will be used in the SVD decomposition (see details)
#' @param value.var name of the column whose values will be used
#' @param n which singular values to return (if \code{NULL}, returns all)
#'
#' @return
#' A list of 3 named elements containing tidy data.tables of the right and left
#' singular vectors, and of their explained variance.
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
#' In the result, the \code{right} and \code{left} singular vectors have a
#' value for each singular value and each combination of the variables
#' used in RHS and LHS of \code{formula}, respectively.
#'
#' It is much faster to compute only some singular vectors, so is advisable not
#' to set n to \code{NULL}. If the irba package is installed, EOF uses
#' [irlba::irlba] instead of [base::svd] since it's much faster.
#'
#' @examples
#' # The Antarctic Oscillation is computed from the
#' # monthly geopotential height anomalies weigthed by latitude.
#' library(data.table)
#' data(geopotential)
#' geopotential <- copy(geopotential)
#' geopotential[, gh.t.w := Anomaly(gh)*sqrt(cos(lat*pi/180)),
#'       by = .(lon, lat, month(date))]
#' aao.svd <- EOF(geopotential, lat + lon ~ date, value.var = "gh.t.w", n = 1)
#'
#' # AAO field
#' library(ggplot2)
#' ggplot(aao.svd$left, aes(lon, lat, z = value)) +
#'     geom_contour(aes(color = ..level..)) +
#'     coord_polar()
#'
#' # AAO signal
#' ggplot(aao.svd$right, aes(date, value)) +
#'     geom_line()
#'
#' # % of explained variance
#' aao.svd$sdev
#'
#' @family meteorology functions
#' @export
#' @import data.table
EOF <- function(data, formula, value.var = guess(data), n = 1) {
    g <- .tidy2matrix(setDT(data), formula, value.var)

    if (is.null(n)) n <- min(ncol(g$matrix), nrow(g$matrix))

    if (requireNamespace("irlba", quietly = TRUE)) {
        set.seed(42)
        eof <- irlba::irlba(g$matrix, nv = max(n), nu = max(n))
    } else {
        eof <- svd(g$matrix, nu = max(n), nv = max(n))
        eof$d <- eof$d[1:max(n)]
    }

    right <- as.data.table(eof$v[, n])
    pcomps <- n
    colnames(right) <- as.character(pcomps)
    right <- cbind(right, as.data.table(g$coldims))
    right <- data.table::melt(right, id.vars = names(g$coldims), variable = "PC")

    left <- as.data.table(eof$u[, n])
    colnames(left) <- as.character(pcomps)
    left <- cbind(left, as.data.table(g$rowdims))
    left <- data.table::melt(left, id.vars = names(g$rowdims), variable = "PC")

    v.g  <- norm(g$matrix, type = "F")
    sdev <- data.table(PC = pcomps, sd = eof$d[n])
    sdev[, r2 := sd^2/v.g^2]

    return(list(right = right, left = left, sdev = sdev))
}
