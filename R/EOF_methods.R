#' Remove some principal components.
#'
#' Returns an `eof` object with just the n principal components.
#'
#' @param x an `eof` object
#' @param n which eofs to keep
#' @param ... further arguments passed to or from other methods
#'
#' @export
cut.eof <- function(x, n, ...) {
    checks <- makeAssertCollection()
    assertClass(x, "eof", add = checks)
    assertIntegerish(n, lower = 1, null.ok = TRUE, add = checks)
    reportAssertions(checks)

    var <- attr(x, "suffix")
    value.var <- attr(x, "value.var")
    return(structure(lapply(as.list(x), function(x) {
        x[as.numeric(get(var)) %in% n]
    }),
    class = c("eof", "list"),
    suffix = var,
    value.var = value.var))
}

#' @export
#' @importFrom stats screeplot
screeplot.eof <- function(x, npcs = "all", type = NULL, main = NULL, ...) {
    checks <- makeAssertCollection()
    assertClass(x, "eof", add = checks)
    assert(checkCharacter(npcs, fixed = "all", any.missing = FALSE, len = 1),
           checkIntegerish(npcs, lower = 1))
    reportAssertions(checks)

    var <- attr(x, "suffix")
    r2 <- "r2"
    if (npcs[1] == "all") npcs <- as.numeric(unique(x$sdev[[var]]))
    ggplot2::ggplot(x$sdev[as.numeric(get(var)) %in% npcs], ggplot2::aes_(as.name(var), as.name(r2))) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes_string(paste0("as.numeric(", var, ")"))) +
        ggplot2::scale_y_continuous(name = expression(R^2))
}

#' @export
#' @importFrom ggplot2 autoplot
autoplot.eof <- function(object, npcs = "all", ...) {
    screeplot(object, npcs, ...)
}

#' @export
#' @importFrom stats predict
predict.eof <- function(object, n = NULL, ...) {
    checks <- makeAssertCollection()
    assertClass(object, "eof", add = checks)
    assertIntegerish(n, lower = 1, null.ok = TRUE, add = checks)
    reportAssertions(checks)

    if(!is.null(n)) object <- cut(object, n)

    value.var <- attr(object, "value.var")
    pc <- attr(object, "suffix")

    right.vars <- colnames(object$right)[!(colnames(object$right) %in% c(pc, value.var))]
    right.formula <- as.formula(paste0(pc, " ~ ", paste0(right.vars, collapse = "+")))

    right <- object$right[object$sdev, on = pc][, (value.var) := get(value.var)*sd]
    right <- .tidy2matrix(right, right.formula, value.var)

    left.vars <- colnames(object$left)[!(colnames(object$left) %in% c(pc, value.var))]
    left.formula <- as.formula(paste0(pc, " ~ ", paste0(left.vars, collapse = "+")))
    left <- .tidy2matrix(object$left, left.formula, value.var)

    dt <- cbind(.extend.dt(left$coldims, each = nrow(right$coldims)),
                .extend.dt(right$coldims, n = nrow(left$coldims)),
                c(Conj(t(right$matrix))%*%left$matrix))
    colnames(dt)[length(colnames(dt))] <- value.var
    return(dt)
}

.extend.dt <- function(dt, n = NULL, each = NULL) {
    if (!is.null(n)) {
        r <- data.table::as.data.table(lapply(dt, rep, n = n))
    } else {
        r <- data.table::as.data.table(lapply(dt, rep, each = each))
    }
    r
}


#' @export
print.eof <- function(x, ...) {
    cat("left:\n")
    print(x$left)
    cat("\nright:\n")
    print(x$right)
    cat("\nsdev:\n")
    print(x$sdev)
}

# #' @export
# `[.eof` <- function(x, left, right, PC) {
#     if (!missing(PC)) {
#         x <- cut(x, PC)
#     }
#     if (!missing(left)) {
#         left <- substitute(left)
#         x$left <- x$left[eval(left), ]
#     }
#     if (!missing(right)) {
#         right <- substitute(right)
#         x$right <- x$right[eval(right), ]
#     }
#
#     x
# }

#' @method summary eof
#' @export
summary.eof <- function(object, ...) {
    cat(gettext("Importance of components:\n"))
    pc <- attr(object, "suffix")
    sdev <- object$sdev[, .(PC = get(pc), sd, r2)]
    sdev[, cum.r2 := cumsum(r2)]
    cat(gettext("Component"), gettext("Explained variance"), gettext("Cumulative variance\n"))
    p <- lapply(seq_len(nrow(sdev)), function(x) {
        cat(formatC(sdev[x, ]$PC, width = 7), "  ",
            formatC(scales::percent(sdev[x, ]$r2), width = 18),
            formatC(scales::percent(sdev[x, ]$cum.r2), width = 19))
        cat("\n")
    })
}


.check_eof <- function(object) {
    if (!inherits(object, "eof")) {
        stopf("'object' must be an EOF object.")
    }
}

if(getRversion() >= "2.15.1") {
    utils::globalVariables("cum.r2")
}


#' Denormalise eof matrices
#'
#' The matrices returned by [EOF()] are normalized. This function multiplies the left or right
#' matrix by the diagonal matrix to return it to proper units.
#'
#' @param eof an `eof` object.
#' @param which which side of the eof decomposition to denormalise
#'
#'
#' @export
denormalise <- function(eof, which = c("left", "right")) {
    .check_eof(eof)
    which <- which[1]
    name <-  attr(eof, "value.var")
    suffix <- attr(eof, "suffix")
    sdev <- eof[["sdev"]]

    value <- eof[[which]][sdev, on = suffix][,  get(name)*sd]

    data.table::copy(eof[[which]])[, (name) := value][]
}


#' @export
#' @rdname denormalise
denormalize <- denormalise
