RG <- function(YA, YB, plot = FALSE) {
  n <- length(YA)

  if (plot == TRUE) {
    plot(1:n/n, 1:n/n, type="l")
    lines(1:n/n, cumsum(sort(YA))/(n*mean(YA)), col = "orange")
    lines(1:n/n, cumsum(sort(YA, TRUE))/(n*mean(YA)), col = "green")
    lines(1:n/n, cumsum(YA[order(YB)])/(n*mean(YA)), col = "red")
  }
  
  return ((sum(YA[order(YB)] * 1:n) - sum(sort(YA, TRUE) * 1:n)) /
            (sum(sort(YA) * 1:n) - sum(sort(YA, TRUE) * 1:n)))
}
