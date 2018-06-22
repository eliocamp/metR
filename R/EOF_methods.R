## Methos for EOF

#' @export
#' @importFrom base cut
cut.eof <- function(eof, n) {
    var <- attr(eof, "suffix")
    value.var <- attr(eof, "value.var")
    return(structure(lapply(as.list(eof), function(x) {
        x[as.numeric(get(var)) %in% n]
    }),
    class = c("eof", "list"),
    suffix = var,
    value.var = value.var))
}

#' @export
#' @importFrom stats screeplot
screeplot.eof <- function(eof, n = "all") {
    var <- attr(eof, "suffix")
    r2 <- "r2"
    if (n[1] == "all") n <- as.numeric(unique(eof$sdev[[var]]))
    ggplot(eof$sdev[as.numeric(get(var)) %in% n], aes_(as.name(var), as.name(r2))) +
        geom_point()
}

#' @export
#' @importFrom ggplot2 autoplot
autoplot.eof <- function(eof, n = "all") {
    screeplot(eof, n)
}

#' @export
#' @importFrom stats predict
predict.eof <- function(eof, n = NULL) {
    ` %>% ` <- magrittr::`%>%`
    if (!inherits(eof, "eof")) {
        stop("eof must be an EOF object")
    }

    if(!is.null(n)) eof <- cut(eof, n)

    value.var <- attr(eof, "value.var")
    pc <- attr(eof, "suffix")

    right.vars <- colnames(eof$right)[!(colnames(eof$right) %in% c(pc, value.var))]
    right.formula <- as.formula(paste0(pc, " ~ ", paste0(right.vars, collapse = "+")))

    right <- eof$right %>%
        .[eof$sdev, on = pc] %>%
        .[, (value.var) := get(value.var)*sd] %>%
        metR:::.tidy2matrix(right.formula, value.var)

    left.vars <- colnames(eof$left)[!(colnames(eof$left) %in% c(pc, value.var))]
    left.formula <- as.formula(paste0(pc, " ~ ", paste0(left.vars, collapse = "+")))
    left <- metR:::.tidy2matrix(eof$left, left.formula, value.var)

    dt <- cbind(.extend.dt(left$coldims, each = nrow(right$coldims)),
                .extend.dt(right$coldims, n = nrow(left$coldims)),
                c(t(right$matrix)%*%left$matrix))
    colnames(dt)[length(colnames(dt))] <- value.var
    return(dt)
}

.extend.dt <- function(dt, n = NULL, each = NULL) {
    if (!is.null(n)) {
        r <- as.data.table(lapply(dt, rep, n = n))
    } else {
        r <- as.data.table(lapply(dt, rep, each = each))
    }
    r
}


#' @export
#' @importFrom base print
print.eof <- function(eof) {
    cat("left:\n")
    print(eof$left)
    cat("\nright:\n")
    print(eof$right)
    cat("\nsdev:\n")
    print(eof$sdev)
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

#' @export
#' @importFrom base summary
summary.eof <- function(eof) {
    cat("Importance of components:\n")
    pc <- attr(eof, "suffix")
    sdev <- eof$sdev[, .(PC = get(pc), sd, r2)]
    sdev[, cum.r2 := cumsum(r2)]
    cat("Component", "Explained variance", "Cumulative variance\n")
    p <- lapply(seq_len(nrow(sdev)), function(x) {
        cat(formatC(sdev[x, ]$PC, width = 7), "  ",
            formatC(scales::percent(sdev[x, ]$r2), width = 18),
            formatC(scales::percent(sdev[x, ]$cum.r2), width = 19))
        cat("\n")
    })
}
