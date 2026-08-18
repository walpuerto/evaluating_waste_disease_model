rg_r <- function(model, x_1, x_2, plot = FALSE) {
  RG(
    predict(model, x_1),
    predict(model, x_2),
    plot
  )
}

z_rgr <- function(model_1, model_2, x_1, x_2) {
  Z_ar(
    predict(model_1, x_1),
    predict(model_1, x_2),
    predict(model_2, x_1),
    predict(model_2, x_2)
  )
}
