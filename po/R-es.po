
# Order 4 Runge kutta for 2d data. static field
runge_kutta4 <- function(x, y, f, h, piece, range.xy, circ.xy){
    # x, y coords
    # f function that takes x and y values and returns the function there
    X <- cbind(x, y)

    k1 <- h*f(X)

    x_plus_k1 <- X + k1/2
    # x_plus_k1[, 1] <- .fold(x_plus_k1[, 1], piece, range.xy[[1]], circ.xy[1])[[1]]
    # x_plus_k1[, 2] <- .fold(x_plus_k1[, 2], piece, range.xy[[2]], circ.xy[2])[[1]]

    k2 <- h*f(x_plus_k1)
    x_plus_k2 <- X + k2/2
    # x_plus_k2[, 1] <- .fold(x_plus_k2[, 1], piece, range.xy[[1]], circ.xy[1])[[1]]
    # x_plus_k2[, 2] <- .fold(x_plus_k2[, 2], piece, range.xy[[2]], circ.xy[2])[[1]]

    k3 <- h*f(x_plus_k2)
    x_plus_k3 <- X + k3
    # x_plus_k3[, 1] <- .fold(x_plus_k3[, 1], piece, range.xy[[1]], circ.xy[1])[[1]]
    # x_plus_k3[, 2] <- .fold(x_plus_k3[, 2], piece, range.xy[[2]], circ.xy[2])[[1]]


    k4 <- h*f(x_plus_k3)


    X_new <- X + k1/6 + k2/3 + k3/3 + k4/6

    # x_new <- .fold(X_new[, 1], piece, range.xy[[1]], circ.xy[1])
    # y_new <- .fold(X_new[, 2], piece, range.xy[[2]], circ.xy[2])

    # changed_piece <- vapply(piece != x_new[[2]] | piece != y_new[[2]], isTRUE, TRUE)

    # piece[changed_piece] <- piece[changed_piece] + 1*sign(h)
    list(x = X_new[, 1],
         y = X_new[, 2])
}
