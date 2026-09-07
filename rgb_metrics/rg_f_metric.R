rg_f <- function(model_1, model_2, x_1, x_2) {
  RG(
    predict(model_1, x_1),
    predict(model_2, x_2)
  )
}

z_rgf <- function(model_1, model_2, x_1, x_2) {
  Z_ef(
    predict(model_1, x_1),
    predict(model_2, x_2)    
  )
}
