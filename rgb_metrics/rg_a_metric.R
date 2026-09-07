rg_a <- function(model, data, plot = FALSE) {
  RG(
    data$cumulative_cases,
    predict(model, data),
    plot
  )
}

z_rga <- function(model_1, model_2, data) {
  Z_ar(
    data$cumulative_cases,
    predict(model_1, data),
    data$cumulative_cases,
    predict(model_2, data)
  )
}
