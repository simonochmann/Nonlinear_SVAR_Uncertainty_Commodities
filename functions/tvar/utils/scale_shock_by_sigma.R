scale_shock_by_sigma <- function(model, e_i, shock_size = 1, A = NULL) {
  # Structural shocks are unit-variance → σ=1; we already convert YAML 'abs'→'sigma' upstream.
  as.numeric(shock_size) * as.numeric(e_i)
}
