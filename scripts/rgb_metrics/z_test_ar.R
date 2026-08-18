delta_hat <- function(Y1M1, Y2M1, Y1M2, Y2M2) {
  n <- length(Y1M1)
  U1 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * Y1M1[order(Y2M1)])
  U2 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * Y1M1[order(Y1M1)])
  U3 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * Y1M2[order(Y2M2)])
  U4 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * Y1M2[order(Y1M2)])
  return (U1/U2 - U3/U4)
}

var_hat_ar <- function(Y1M1, Y2M1, Y1M2, Y2M2) {
  n <- length(Y1M1)
  D <- sapply(
    1:n,
    \(i) delta_hat(Y1M1[-i], Y2M1[-i], Y1M2[-i], Y2M2[-i])
  )
  return ((n-1)/n *sum((D-mean(D))^2))
}

Z_ar <- function(Y1M1, Y2M1, Y1M2, Y2M2) {
  delta_hat(Y1M1, Y2M1, Y1M2, Y2M2) / sqrt(var_hat_ar(Y1M1, Y2M1, Y1M2, Y2M2))
}
