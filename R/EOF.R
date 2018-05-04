#' Empirical Orthogonal Function
#'
#' Computes Singular Value Decomposition (also known as Principal Components
#' Analysis or Empirical Orthogonal Functions).
#'
#' @param data a data.frame
#' @param formula a formula passed to \code{\link[data.table]{dcast}} to build
#' the matrix that will be used in the SVD decomposition (see Details)
#' @param value.var optional name of the data column (see Details)
#' @param n which singular values to return (if \code{NULL}, returns all)
#' @param B number of bootstrap samples used to estimate confidence intervals.
#' Ignored if <= 1.
#' @param probs the probabilities of the lower and upper values of estiamted
#' confidence intervals. If named, it's names will be used as column names.
#'
#' @return
#' A list of 3 named elements containing tidy data.tables of the right and left
#' singular vectors, and of their explained variance.
#'
#' @details
#' Singular values can be computed over matrices so \code{formula} denotes how
#' to build a matrix from the data. It is a formula of the form VAR ~ LEFT | RIGHT
#' (see [Formula::Formula]) in which VAR is the variable whose values will
#' populate the matrix, and LEFT represent the variables used to make the rows
#' and RIGHT, the columns of the matrix.
#' Think it like "VAR *as a function* of LEFT *and* RIGHT".
#'
#' Alternatively, if `value.var` is not `NULL`, it's possible to use the
#' (probably) more familiar [data.table::dcast] formula interface. In that case,
#' `data` must be provided.
#'
#' The result of VAR ~ LHS | RHS and VAR ~ RHS | LHS (ie, terms reversed)
#' is the same, with the exception that the order of the singular vector is
#' reversed (right is left and left is right).
#'
#' The variable combination used in this formula *must* identify
#' an unique value in a cell. For the time being, no error will be raised, but
#' there will be a message from \code{\link[data.table]{dcast}}.
#'
#' In the result, the \code{right} and \code{left} singular vectors have a
#' value for each singular value and each combination of the variables
#' used in RIGHT and LEFT of \code{formula}, respectively.
#'
#' It is much faster to compute only some singular vectors, so is advisable not
#' to set n to \code{NULL}. If the irba package is installed, EOF uses
#' [irlba::irlba] instead of [base::svd] since it's much faster.
#'
#' The boostraping procedure follows Fisher et.al. (2016) and returns the
#' standard deviation of each singular value.
#'
#' @examples
#' # The Antarctic Oscillation is computed from the
#' # monthly geopotential height anomalies weigthed by latitude.
#' library(data.table)
#' data(geopotential)
#' geopotential <- copy(geopotential)
#' geopotential[, gh.t.w := Anomaly(gh)*sqrt(cos(lat*pi/180)),
#'       by = .(lon, lat, month(date))]
#'
#' aao <- EOF(gh.t.w ~ lat + lon | date, data = geopotential, n = 1,
#'            B = 100, probs = c(lo2 = 0.1, hig = 0.9))
#'
#' # AAO field
#' library(ggplot2)
#' ggplot(aao$left, aes(lon, lat, z = gh.t.w)) +
#'     geom_contour(aes(color = ..level..)) +
#'     coord_polar()
#'
#' # AAO signal
#' ggplot(aao$right, aes(date, gh.t.w)) +
#'     geom_line()
#'
#' # standard deviation, % of explained variance and
#' # confidence intervals.
#' aao$sdev
#'
#' # 1st eof for each month.
#' aao2 <- geopotential[, EOF(gh.t.w ~ lat + lon | date, n = 1)$left, by = month(date)]
#'
#' ggplot(aao2, aes(lon, lat)) +
#'     geom_contour(aes(z = gh.t.w, color = ..level..)) +
#'     facet_wrap(~ month)
#'
#' # Alternative interface
#' aao2 <- EOF(lon + lat ~ date, value.var = "gh.t.w", data = geopotential)
#'
#' @references
#' Fisher, A., Caffo, B., Schwartz, B., & Zipunnikov, V. (2016). Fast, Exact Bootstrap Principal Component Analysis for p > 1 million. Journal of the American Statistical Association, 111(514), 846–860. http://doi.org/10.1080/01621459.2015.1062383
#' @family meteorology functions
#' @export
#' @import data.table
#' @import Formula
#' @importFrom stats as.formula
EOF <- function(formula, value.var = NULL, data = NULL, n = 1, B = 0,
                probs = c(lower = 0.025, mid = 0.5, upper = 0.975)) {

    if (!is.null(value.var)) {
        if (is.null(data)) stop("data must not be NULL if value.var is NULL",
                                .call = FALSE)
        data <- copy(data)
        f <- as.character(formula)
        f <- stringr::str_replace(f, "~", "\\|")
        formula <- Formula::as.Formula(paste0(value.var, " ~ ", f))
    }

    if (is.null(data)) {
        formula <- Formula::as.Formula(formula)
        data <- as.data.table(eval(quote(model.frame(formula, data  = data))))
    }

    f <- as.character(formula)
    f <- stringr::str_split(f,"~", n = 2)[[1]]
    dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
    dcast.formula <- as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))

    value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

    g <- .tidy2matrix(data, dcast.formula, value.var)

    if (is.null(n)) n <- min(ncol(g$matrix), nrow(g$matrix))


    if (requireNamespace("irlba", quietly = TRUE)) {
        eof <- irlba::irlba(g$matrix, nv = max(n), nu = max(n), rng = runif)
    } else {
        eof <- svd(g$matrix, nu = max(n), nv = max(n))
        eof$d <- eof$d[1:max(n)]
    }

    right <- as.data.table(eof$v[, n])
    pcomps <- paste0("PC", n)
    colnames(right) <- pcomps
    right <- cbind(right, as.data.table(g$coldims))
    right <- data.table::melt(right, id.vars = names(g$coldims), variable = "PC",
                              value.name = value.var)
    right[, PC := factor(PC, levels = pcomps, ordered = TRUE)]

    left <- as.data.table(eof$u[, n])
    colnames(left) <- pcomps
    left <- cbind(left, as.data.table(g$rowdims))
    left <- data.table::melt(left, id.vars = names(g$rowdims), variable = "PC",
                             value.name = value.var)
    left[, PC := factor(PC, levels = pcomps, ordered = TRUE)]

    v.g  <- norm(g$matrix, type = "F")
    sdev <- data.table(PC = pcomps, sd = eof$d[n])
    sdev[, PC := factor(PC, levels = pcomps, ordered = TRUE)]
    sdev[, r2 := sd^2/v.g^2]

    if (B > 1) {
        tall <- dim(g$matrix)[1] > dim(g$matrix)[2]
        set.seed(42)
        if (!tall) {
            names(eof) <- c("d", "v", "u", "iter", "mprod")
        }
        # if (tall) {
            p <- nrow(eof$v)
            sdevs <- lapply(seq_len(B), function(x) {
                Prow <- sample(seq_len(p), replace = TRUE)
                m <- diag(eof$d, nrow = max(n), ncol = max(n))%*%t(eof$v)[, Prow]
                return(svd(m)$d)
            })
        # } else {
        #     p <- ncol(eof$u)
        #     sdevs <- lapply(seq_len(B), function(x) {
        #         Prow <- sample(seq_len(p), replace = TRUE)
        #         m <- eof$u%*%diag(eof$d, nrow = max(n), ncol = max(n))[Prow, ]
        #         return(svd(m)$d)
        #     })
        # }
        se <- lapply(data.table::transpose(sdevs), quantile, probs = probs, names = FALSE)
        se <- data.table::transpose(se)
        # m <- unlist(lapply(data.table::transpose(sdevs), mean))
        if (is.null(names(probs))) names(probs) <- scales::percent(probs)
        sdev[, names(probs) := se]
    }

    return(list(right = right, left = left, sdev = sdev))
}
