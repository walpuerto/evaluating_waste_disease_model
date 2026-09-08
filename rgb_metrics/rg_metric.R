construct_curve <- function(n, YA, algorithm, YB = NULL) {
  tibble(
    x = 1:n/n,
    y = algorithm
  ) |> add_row(
    x = 0,
    y = 0,
    .before = 1
  )
}


RG <- function(YA, YB, plot = FALSE) {
  
  # Make it easier for us to plot later, and cleaner too
  n <- length(YA)
  lorenz_curve <- construct_curve(
    n,
    YA,
    cumsum(sort(YA))/(n*mean(YA))
  )
  
  dual_lorenz_curve <- construct_curve(
    n,
    YA,
    cumsum(sort(YA, TRUE))/(n*mean(YA))
  )
  
  concordance_curve <- construct_curve(
    n,
    YA,
    # Arrange YA according to the ranks of YB
    cumsum(YA[order(rank(YB))])/(n*mean(YA)),
    YB
  )
  
  # If the user wants to quickly confirm or validate their results
  if (plot == TRUE) {
    plot(
      0:n/n,
      0:n/n,
      type = "l",
      xlab = "",
      ylab = ""
    )
    lines(
      lorenz_curve$x,
      lorenz_curve$y,
      col = "orange"
    )
    lines(
      dual_lorenz_curve$x,
      dual_lorenz_curve$y,
      col = "green"
    )
    lines(
      concordance_curve$x,
      concordance_curve$y,
      col = "red"
    )
  }
  
  sum(dual_lorenz_curve$y - concordance_curve$y)/
    sum(dual_lorenz_curve$y - lorenz_curve$y)
}
