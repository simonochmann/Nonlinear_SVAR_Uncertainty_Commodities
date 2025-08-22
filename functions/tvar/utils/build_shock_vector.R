build_shock_vector <- function(model, impulse) {
  k <- length(model$variables)
  e <- rep(0, k); e[match(impulse, model$variables)] <- 1
  e
}
