gamma_hat <- function(YA, YB) {
  n <- length(YA)
  U1 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * YA[order(YA)])
  U2 <- 1/(4 * choose(n, 2)) * sum((2*1:n - 1 - n) * YA[order(YB)])
  return (U1 - U2)
}

var_hat_ef <- function(YA, YB) {
  n <- length(YA)
  G <- sapply(
    1:n,
    \(i) gamma_hat(YA[-i], YB[-i])
  )
  return((n - 1)/n * sum((G - mean(G))^2))
}

Z_ef <- function(YA, YB) {
  gamma_hat(YA, YB) / sqrt(var_hat_ef(YA, YB))
}
