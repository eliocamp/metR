`[.circular` <- function(x, i, j, drop) {
    if (is.numeric(i)) {
        N <- length(x)
        i <- i[i != 0]
        i <- ifelse(i < 0, i %% N + 1, i)
        i <- ifelse(i > N, i %% N, i)
    }
    r <- unclass(x)
    r1 <- get.range(x)
    r <- r[i]
    class(r) <- oldClass(x)
    r <- set.range(r, r1)
    return(r)
}

# circular <- function(x, range = NA) {
#     class(x) <- c(class(x), "circular")
#     x <- set.range(x, range)
#     # fold(x)
# }

# x <- circular(1:360, range = c(0, 360))

fold.circular <- function(x) {
    r <- get.range(x)
    dr <- diff(r)
    x <- ifelse(x >= r[2] | x < r[1], x %% dr, x)
    class(x) <- c(class(x), "circular")
    x <- set.range(x, r)
    x
}

fold <- function(...) {
    UseMethod("fold")
}

set.range <- function(x, range) {
    attr(x, "range") <- range
    x
}
get.range.circular <- function(x) {
    attr(x, "range")
}

get.range <- function(x) {
    UseMethod("get.range")
}

`-.circular` <- function(x, y) {
    stopifnot(identical(get.range(x), get.range(y)))
    r <- get.range(x)
    r1 <- x
    r2 <- y
    class(r1) <- class(x)[!class(x) %in% "circular"]
    class(r2) <- class(y)[!class(y) %in% "circular"]
    r1 <- as.numeric(r1) - as.numeric(r2)
    class(r1) <- oldClass(x)
    r1 <- set.range(r1, r)
    r1 <- fold(r1)
    r1
}

`-.circular` <- function(x, y) {
    stopifnot(identical(get.range(x), get.range(y)))
    r <- get.range(x)
    r1 <- x
    r2 <- y
    class(r1) <- class(x)[!class(x) %in% "circular"]
    class(r2) <- class(y)[!class(y) %in% "circular"]
    r1 <- as.numeric(r1) - as.numeric(r2)
    class(r1) <- oldClass(x)
    r1 <- set.range(r1, r)
    r1
}

diff.circular <- function (x, lag = 1L, differences = 1L, ...) {
    xlen <- length(x)
    if (length(lag) != 1L || length(differences) > 1L || differences < 1L)
        stop("'lag' and 'differences' must be integers >= 1")
    select <- seq_len(xlen) + lag
    select[select == 0] <- xlen

    for (i in seq_len(differences)) {
        x <- fold(x[select] - x)
    }
    x
}
