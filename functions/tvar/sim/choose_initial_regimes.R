# functions/tvar/sim/choose_initial_regimes.R
choose_initial_regimes <- function(model, B, mode = c("low","high","mix")) {
  mode <- match.arg(mode)
  if (mode != "mix") return(rep(mode, B))
  
  # robust getter
  get_num <- function(x) if (!is.null(x) && is.finite(as.numeric(x)[1])) as.numeric(x)[1] else NULL
  meta <- if (!is.null(model$metadata) && is.list(model$metadata)) model$metadata else NULL
  
  p_low <- NULL
  # try multiple likely slots
  p_low <- p_low %||% get_num(if (!is.null(meta)) meta$start_prob_low)
  p_low <- p_low %||% get_num(if (!is.null(meta)) meta$low_prob)
  p_low <- p_low %||% get_num(model$start_prob_low)
  p_low <- p_low %||% get_num(if (!is.null(model$regime_probs)) model$regime_probs$low)
  p_low <- p_low %||% get_num(if (!is.null(model$initial_regime_probs)) model$initial_regime_probs$low)
  p_low <- p_low %||% 0.5
  p_low <- min(max(p_low, 0), 1)
  
  ifelse(stats::runif(B) < p_low, "low", "high")
}
