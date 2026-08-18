rg_e <- function(model_1, model_2, x_1, x_2) {
  1 - RG(
    predict(model_1, x_1),
    predict(model_2, x_2)
  )
}

z_rge <- function(model_1, model_2, x_1, x_2) {
  Z_ef(
    predict(model_1, x_1),
    predict(model_2, x_2)
  )
}
