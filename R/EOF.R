#' Empirical Orthogonal Function
#'
#' Computes Singular Value Decomposition (also known as Principal Components
#' Analysis or Empirical Orthogonal Functions).
#'
#' @param data a data.frame
#' @param formula a formula to build the matrix that will be used in the SVD
#' decomposition (see Details)
#' @param n which singular values to return (if \code{NULL}, returns all)
#' @param B number of bootstrap samples used to estimate confidence intervals.
#' Ignored if <= 1.
#' @param probs the probabilities of the lower and upper values of estimated
#' confidence intervals. If named, it's names will be used as column names.
#' @param rotate if `TRUE`, scores and loadings will be rotated using [varimax]
#' @param suffix character to name the principal components
#' @param fill value to infill implicit missing values or `NULL` if the
#' data is dense.
#' @param engine function to use to compute SVD. If `NULL` it uses [irlba::irlba]
#' (if installed) if the largest singular value to compute is lower than half the maximum
#' possible value, otherwise it uses [base::svd]. If the user provides a function,
#' it needs to be a drop-in replacement for [base::svd] (the same arguments and
#' output format).
#'
#' @return
#' An `eof` object which is just a named list of `data.table`s
#' \describe{
#'    \item{left}{data.table with left singular vectors}
#'    \item{right}{data.table with right singular vectors}
#'    \item{sdev}{data.table with singular values, their explained variance,
#'    and, optionally, quantiles estimated via bootstrap}
#' }
#'
#' There are some methods implemented
#' * [summary]
#' * [screeplot] and the equivalent [autoplot]
#' * [cut.eof]
#' * [predict]
#'
#' @details
#' Singular values can be computed over matrices so \code{formula} denotes how
#' to build a matrix from the data. It is a formula of the form VAR ~ LEFT | RIGHT
#' (see [Formula::Formula]) in which VAR is the variable whose values will
#' populate the matrix, and LEFT represent the variables used to make the rows
#' and RIGHT, the columns of the matrix. Think it like "VAR *as a function* of
#' LEFT *and* RIGHT". The variable combination used in this formula *must* identify
#' an unique value in a cell.
#'
#' So, for example, `v ~ x + y | t` would mean that there is one value of `v` for
#' each combination of `x`, `y` and `t`, and that there will be one row for
#' each combination of `x` and `y` and one row for each `t`.
#'
#' In the result, the left and right vectors have dimensions of the LEFT and RIGHT
#' part of the `formula`, respectively.
#'
#' It is much faster to compute only some singular vectors, so is advisable not
#' to set n to \code{NULL}. If the irlba package is installed, EOF uses
#' [irlba::irlba] instead of [base::svd] since it's much faster.
#'
#' The bootstrapping procedure follows Fisher et.al. (2016) and returns the
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
#' eof <- EOF(gh.t.w ~ lat + lon | date, 1:5, data = geopotential,
#'            B = 100, probs = c(low = 0.1, hig = 0.9))
#'
#' # Inspect the explained variance of each component
#' summary(eof)
#' screeplot(eof)
#'
#' # Keep only the 1st.
#' aao <- cut(eof, 1)
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
#' # Reconstructed fields based only on the two first
#' # principal components
#' field <- predict(eof, 1:2)
#'
#' # Compare it to the real field.
#' ggplot(field[date == date[1]], aes(lon, lat)) +
#'     geom_contour_fill(aes(z = gh.t.w), data = geopotential[date == date[1]]) +
#'     geom_contour2(aes(z = gh.t.w, linetype = factor(-sign(stat(level))))) +
#'     scale_fill_divergent()
#'
#'
#' @references
#' Fisher, A., Caffo, B., Schwartz, B., & Zipunnikov, V. (2016). Fast, Exact Bootstrap Principal Component Analysis for p > 1 million. Journal of the American Statistical Association, 111(514), 846–860. \doi{10.1080/01621459.2015.1062383}
#' @family meteorology functions
#' @export
EOF <- function(formula, n = 1, data = NULL, B = 0,
                probs = c(lower = 0.025, mid = 0.5, upper = 0.975),
                rotate = FALSE, suffix = "PC", fill = NULL,
                engine = NULL) {
    checks <- makeAssertCollection()

    assertClass(formula, "formula", add = checks)
    assertDataFrame(data, null.ok = TRUE, add = checks)
    assertIntegerish(n, lower = 1, null.ok = TRUE, add = checks)
    assertCount(B, add = checks)
    assertNumeric(probs, lower = 0, upper = 1, any.missing = FALSE, add = checks)
    assertCharacter(names(probs), unique = TRUE, any.missing = FALSE,
                    null.ok = TRUE, min.chars = 1,
                    add = checks)
    assertFlag(rotate, add = checks)
    assertCharacter(suffix, len = 1, any.missing = FALSE, add = checks)
    assertNumber(fill, finite = TRUE, null.ok = TRUE, add = checks)

    reportAssertions(checks)

    f <- as.character(formula)

    if (length(f) == 1) {  # formula.tool did its thing
        f <- stringr::str_split(f, "~", n = 2)[[1]]
    } else {
        f <- f[-1]
    }

    value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

    matrix.vars <- f[stringr::str_detect(f, "\\|")]
    matrix.vars <- stringr::str_split(matrix.vars,"\\|", n = 2)[[1]]

    row.vars <- stringr::str_squish(stringr::str_split(matrix.vars[1], "\\+")[[1]])
    col.vars <- stringr::str_squish(stringr::str_split(matrix.vars[2], "\\+")[[1]])

    if (is.null(data)) {
        formula <- Formula::as.Formula(formula)
        data <- data.table::as.data.table(eval(quote(model.frame(formula, data  = data))))
    } else {
        # Check if columns are indata
        all.cols <- c(value.var, row.vars, col.vars)
        missing.cols <- all.cols[!(all.cols %in% colnames(data))]
        if (length(missing.cols) != 0) {
            stopf("Columns not found in data: %s.", paste0(missing.cols, collapse = ", "))
        }
        data <- data.table::setDT(data)[, (all.cols), with = FALSE]
    }

    data.table::setDT(data)
    dcast.formula <- stringr::str_squish(f[stringr::str_detect(f, "\\|")])
    dcast.formula <- stats::as.formula(stringr::str_replace(dcast.formula, "\\|", "~"))
    value.var <- stringr::str_squish(f[!stringr::str_detect(f, "\\|")])

    g <- .tidy2matrix(data, dcast.formula, value.var, fill = fill)

    if (length(g$matrix) < nrow(data)) {
        stopf("The formula %s does not identify an unique observation for each cell.",  as.character(formula))
    }

    tall <- dim(g$matrix)[1] > dim(g$matrix)[2]
    v.g  <- norm(abs(g$matrix), type = "F")

    if (is.null(n)) n <- seq_len(min(ncol(g$matrix), nrow(g$matrix)))

    if (is.null(engine)) {
        if (requireNamespace("irlba", quietly = TRUE) &
            max(n) < 0.5 *  min(ncol(g$matrix), nrow(g$matrix))) {
            engine <- irlba_engine
        } else {
            engine <- base::svd
        }
    } else {
        engine <- match.fun(engine)
    }

    eof <- engine(g$matrix, nu = max(n), nv = max(n))
    eof$d <- eof$d[n]

    remove(data)
    gc()
    pcomps <- paste0(suffix, n)
    if (rotate == TRUE & max(n) > 1) {
        # Rotation
        eof$D <- diag(eof$d, ncol = max(n), nrow = max(n))
        loadings <- t(with(eof, D%*%t(v)))
        scores <- eof$u
        R <- stats::varimax(loadings, normalize = FALSE)
        eof$u <- eof$u%*%R$rotmat

        # Recover rotated V and D matrixs
        loadings <- R$loadings
        class(loadings) <- "matrix"
        eof$d <- sqrt(apply(loadings, 2, function(x) sum(x^2)))
        eof$v <- t(diag(1/eof$d)%*%t(loadings))
    }

    # setDF(data)
    right <- cbind(data.table::data.table(rep(pcomps, each = nrow(eof$v))), c(eof$v[, n]))
    colnames(right) <- c(suffix, value.var)
    right <- cbind(unique(g$coldims), right)
    right[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]

    left <- cbind(data.table::data.table(rep(pcomps, each = nrow(eof$u))), c(eof$u[, n]))
    colnames(left) <- c(suffix, value.var)
    left <- cbind(unique(g$rowdims), left)
    left[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]

    # setDT(data)
    r2 <- eof$d^2/v.g^2
    sdev <- data.table::data.table(pcomps, eof$d)
    colnames(sdev) <- c(suffix, "sd")
    sdev[, (suffix) := factor(get(suffix), levels = pcomps, ordered = TRUE)]
    sdev[, r2 := r2]

    if (B > 1) {
        set.seed(42)
        if (!tall) {
            names(eof)[1:3] <- c("d", "v", "u")
        }
        loadings <- with(eof, diag(d, ncol = max(n), nrow = max(n))%*%t(v))
        p <- nrow(eof$v)
        sdevs <- lapply(seq_len(B), function(x) {
            Prow <- sample(seq_len(p), replace = TRUE)
            m <- loadings[, Prow]
            eof <- svd(m)
            if (rotate == TRUE) {
                loadings <- t(with(eof, diag(d, ncol = max(n), nrow = max(n))%*%t(v)))
                R <- stats::varimax(loadings, normalize = FALSE)
                loadings <- R$loadings
                class(loadings) <- "matrix"
                return(sqrt(apply(loadings, 2, function(x) sum(x^2))))
            } else {
                return(svd(m)$d)
            }
        })

        se <- lapply(data.table::transpose(sdevs), stats::quantile, probs = probs, names = FALSE)
        se <- data.table::transpose(se)
        if (is.null(names(probs))) names(probs) <- scales::percent(probs)
        sdev[, names(probs) := se]
    }


    return(structure(list(left = left, right = right, sdev = sdev),
                     call = match.call(),
                     class = c("eof", "list"),
                     suffix = suffix,
                     value.var = value.var,
                     engine = engine))
}

irlba_engine <- function(A, nv, nu) irlba::irlba(A, nv, nu, rng = runif)
