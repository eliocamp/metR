# Order 4 Runge kutta for 2d data. static field
runge_kutta4 <- function(x, y, f, h) {
  # x, y coords
  # f function that takes x and y values and returns the function there
  X <- cbind(x, y)

  k1 <- h * f(X)

  x_plus_k1 <- X + k1 / 2

  k2 <- h * f(x_plus_k1)
  x_plus_k2 <- X + k2 / 2

  k3 <- h * f(x_plus_k2)
  x_plus_k3 <- X + k3

  k4 <- h * f(x_plus_k3)

  X_new <- X + k1 / 6 + k2 / 3 + k3 / 3 + k4 / 6

  list(x = X_new[, 1], y = X_new[, 2])
}
